package com.metl.datamigrator
import java.time.ZoneId
import java.util.{Date, GregorianCalendar}

import com.metl.data._
import com.metl.metl2011._
import com.metl.h2._
import net.liftweb.common._
import net.liftweb.actor._
import net.liftweb.util._
import net.liftweb.util.Helpers._

import scala.xml._
import dispatch._
import Defaults._
import com.metl.utils.Stopwatch
import com.metl.MultiFormatDateFormatter

case class AddKey(h:String,k:String,v:String)
case class RemoveKey(h:String,k:String,v:String)
class MaintainKeys(start:Long) extends LiftActor with Logger {
  def shutdown = {
    shouldCheck = false
  }
  protected var shouldCheck = true
  case object Ping
  var keys:List[Tuple3[String,String,String]] = Nil
  def repeat = {
    if (shouldCheck){
      Schedule.schedule(this,Ping,10 seconds)
    }
  }
  def messageHandler = {
    case AddKey(host,key,value) => {
      keys = (host,key,value) :: keys
      repeat
    }
    case RemoveKey(host,key,value) => {
      keys = keys.filterNot(k => k._1 == host && k._2 == key && k._3 == value)
      repeat
    }
    case Ping => {
      if (shouldCheck) {
        keys.foreach(k => {
          val svc = url("%s/authenticationState".format(k._1)).GET <:< Map("Cookie" -> "%s=%s".format(k._2, k._3))
          val result = Http(svc OK as.String).either.right.map(r => (XML.loadString(r) \\ "@id").exists(_.text.toString == "authData"))
          info("[%sms] maintainingKey: %s".format(new java.util.Date().getTime - start, result()))
        })
        repeat
      }
    }
  }
}

object Main extends App with Logger {
  def generatePrivateListFunc(config:ServerConfiguration):Conversation=>Option[List[String]] = {
      config match {
        /*
        case cm2011:MeTL2011BackendAdaptor => (conv:Conversation) => {
          None // this is where the parse through the content will occur, so that we can determine who's made private or public actions in these conversations, so that we don't lose their content.
          val rootHost = cm2011.host
        }
        */
        case sa:ReadOnlyMeTL2011ZipAdaptor => {
          (conv:Conversation) => Some((sa.getPrivateAuthorsForConversation(conv.jid.toString) ::: sa.getHistory(conv.jid.toString).getAll.map(_.author)).distinct)
        }
        case _ => (conv:Conversation) => None
      }
    }
    /*
    val runningFromConsole:Boolean = args.toList.exists(_.split("=") match {
      case Array("runningFromConsole", value) => value.toLowerCase.trim.toBoolean == true
      case _ => false
    })
    */
    val start = new java.util.Date().getTime
    val maintainKeys = new MaintainKeys(start)
    def mark(msg:String) = {
      info("[%sms] %s".format(new java.util.Date().getTime - start,msg))
    }
    val configurationFileLocation = System.getProperty("metlx.configurationFile")
    MeTL2011ServerConfiguration.initialize
    MeTL2015ServerConfiguration.initialize
    LocalH2ServerConfiguration.initialize
    ServerConfiguration.addServerConfigurator(ReadOnlyMeTL2011ZipAdaptorConfigurator)
    ServerConfiguration.loadServerConfigsFromFile(
      path = configurationFileLocation,
      onConversationDetailsUpdated = (c:Conversation) => {},
      messageBusCredentailsFunc = () => ("",""),
      conversationListenerCredentialsFunc = () => ("",""),
      httpCredentialsFunc = () => ("","")
    )
    val servers = ServerConfiguration.getServerConfigurations
    val configFile = XML.load(configurationFileLocation)
    val parallelism = try {
      Some((configFile \\ "parallelism").headOption.map(_.text.toInt))
    } catch {
      case e:Exception => None
    }
    val importDescription = for {
      idNode <- configFile \\ "importDescription"
    } yield {
      idNode.text
    }
    val sortConversations:List[Conversation]=>List[Conversation] = {
      val priorityElems = (configFile \\ "priorities" \  "priority")
      val doAllAfter = (configFile \\ "priorities" \ "@completeLoadAfterPriorities").headOption.map(_.text.toBoolean).getOrElse(true)
      val jidPriorities = priorityElems.flatMap(elem => (elem \ "@jid").headOption.map(_.text.toInt))
      val authorPriorities = priorityElems.flatMap(elem => (elem \ "@author").headOption.map(_.text))
      (in:List[Conversation]) => {
        val (jidMatches,nonJid) = in.sortWith((a,b) => a.lastAccessed > b.lastAccessed).partition(c => jidPriorities.contains(c.jid))
        val (authorMatches,nonAuthor) = nonJid.partition(c => authorPriorities.contains(c.author))
        jidMatches ::: authorMatches ::: nonAuthor.filter(_i => doAllAfter)
      }
    }
    val (targetServer,cookieKey,cookieValue) = (configFile \\ "targetServer").headOption.map(cn => {
        (
          cn.text,
          (cn \\ "@cookieKey").headOption.map(_.text).getOrElse("JSESSIONID"),
          (cn \\ "@cookieValue").headOption.map(_.text).getOrElse({
            throw new Exception("please specify the target server's cookie in the configuration file")
          })
        )
      }).getOrElse({
        throw new Exception("please specify the target server's baseUrl in the configuration file")
      })
    maintainKeys ! AddKey(targetServer,cookieKey,cookieValue)
    mark("servers: %s => %s".format(servers,targetServer))
    servers.filterNot(_ == EmptyBackendAdaptor).foreach(config => {
      mark("exporting server: %s".format(config))
      config.isReady
      val timezoneOverrides = (for {
        dtf <- configFile \\ "dateTimeFormat"
        format <- (dtf \ "@format").headOption.map(_.text).filterNot(_ == "")
        zoneId = (dtf \ "@zoneId").headOption.map(_.text).filterNot(_ == "")
      } yield {
        zoneId match {
          case Some(z) => Right((format,z))
          case None => Left(format)
        }
      }).toList
      val exportSerializer = new MigratorXmlSerializer(config.name,timezoneOverrides)
      val privateFunc = generatePrivateListFunc(config)
      def exportConversation(onBehalfOfUser:String,conversation:String):Box[NodeSeq] = {
        tryo((for (
          conv <- Some(config.detailsOfConversation(conversation));
          if (onBehalfOfUser == conv.author);
          histories = exportHistories(conv,privateFunc(conv));
          xml = {
            <export>
            {exportSerializer.fromConversation(conv)}
            <histories>{histories.map(h => exportSerializer.fromHistory(h))}</histories>
            </export>
          }
        ) yield {
          xml
        }).head)
      }
      def time[A](label:String,action: => A):A = {
        val s = new java.util.Date().getTime
        val res = action
        info(label + "(%s ms)".format(new java.util.Date().getTime - s))
        res
      }
      def exportHistories(conversation:Conversation,restrictToPrivateUsers:Option[List[String]]):List[History] = {
        var convHistory = time("getHistory(%s)".format(conversation.jid),config.getHistory(conversation.jid.toString).filter(m => {
          restrictToPrivateUsers.map(users => {
            m match {
              case q:MeTLQuiz => true
              case mcc:MeTLCanvasContent => mcc.privacy == Privacy.PUBLIC || users.contains(mcc.author)
              case ms:MeTLStanza => users.contains(ms.author)
            }
          }).getOrElse(true)
        }))
        val attendees = convHistory.getAttendances.map(_.author).distinct
        val participants = (attendees ::: restrictToPrivateUsers.getOrElse(List.empty[String])).distinct.filter(u => restrictToPrivateUsers.map(_.contains(u)).getOrElse(true))
        val histories = conversation.slides.flatMap(slide => {
          val slideJid = slide.id.toString
          val publicHistory = config.getHistory(slideJid)
          val privateHistories = participants.map(p => {
            config.getHistory(slideJid + p)
          })
          val allHistories = publicHistory :: privateHistories
          allHistories.flatMap(_.getAll).groupBy(_.author).foreach(authorStanzas => {
            if (!attendees.contains(authorStanzas._1)){
              val times = authorStanzas._2.map(_.timestamp)
              val entering = Attendance(EmptyBackendAdaptor,authorStanzas._1,times.min,slideJid,true,Nil)
              val leaving = Attendance(EmptyBackendAdaptor,authorStanzas._1,times.max,slideJid,true,Nil)
              convHistory.addAttendance(entering)
              convHistory.addAttendance(leaving)
            }
          })
          allHistories
        })
        convHistory :: histories
      }
      val rawConvs = config.searchForConversation("")//.take(25) // hopefully every conversation will be returned by this query?  Will probably have to write an explicit "getAllConversations" call into the backends.
      val convs = sortConversations(rawConvs)
      mark("fetched conversations: %s => %s".format(rawConvs.length, convs.length))
      val p = convs.par
      parallelism.map(paraCount => {
        p.tasksupport = new scala.collection.parallel.ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(paraCount.getOrElse(1)))
      })
      val completed = p.map(conversation => {
        val es = new java.util.Date().getTime
        time("exporting conversation",exportConversation(conversation.author,conversation.jid.toString)).map(xml => {
          val ee = new java.util.Date().getTime
          mark("exported conversation: %s (%s) %s".format(conversation.author, conversation.slides.length, conversation.title))
          val is = new java.util.Date().getTime
          val svc = url("%s/conversationImport".format(targetServer)).POST << xml.toString <:< Map ("Cookie" -> "%s=%s".format(cookieKey, cookieValue)) <<? Map (importDescription.map(id => ("importDescription",id)).toList :_*)

          val result = Http(svc OK as.xml.Elem).either
          val res = result()
          //val res = Left(new Exception("deliberately failing"))
          val ie = new java.util.Date().getTime
          val exportTime = ee - es
          val importTime = ie - is
          val total =  ie - es
          res.right.map(crx => {
            mark("successful import: %s (%s) %s => (%sB)".format(conversation.author,conversation.slides.length,conversation.title,crx.toString.length))
          }).left.map(e => {
            error("exception while pushing conversation: %s (%s) %s: %s".format(conversation.author,conversation.slides.length,conversation.title,e.getMessage),e)
          })
          (conversation.jid,conversation.created,res.isRight,exportTime,importTime,total)
        }) match {
          case Full(r) => r
          case Empty => {
            val now = new java.util.Date().getTime
            error("exception %s while generating export xml, resulting in empty".format(conversation.jid))
            (conversation.jid,conversation.created,false,es,now,now - es)
          }
          case ParamFailure(msg,exception,chain,param) => {
            val now = new java.util.Date().getTime
            error("exception %s while generating export xml: %s (%s)\r\n%s => %s".format(
              conversation.jid,
              msg,
              param,
              exception.map(_.getMessage),
              exception.map(_.getStackTraceString)
            ))
            (conversation.jid,conversation.created,false,es,now,now - es)
          }
          case Failure(msg,exception,chain) => {
              val now = new java.util.Date().getTime
              error("exception %s while generating export xml: %s\r\n%s => %s".format(
                conversation.jid,
                msg,
                exception.map(_.getMessage),
                exception.map(_.getStackTraceString)
              ))
              (conversation.jid,conversation.created,false,es,now,now - es)
          }
        }
      }).toList
      mark("completed pushing conversations: \r\n%s".format(completed.map(c => "CONV %s @ '%s' [%s (%s + %s)]: %s".format(c._1,c._2,c._6,c._4,c._5,c._3)).mkString("\r\n")))
      maintainKeys ! RemoveKey(targetServer,cookieKey,cookieValue)
      config.shutdown
    })
    maintainKeys.shutdown
    Schedule.shutdown()
    LAScheduler.shutdown()
    mark("finished reading.")
    System.exit(0)
}

class MigratorXmlSerializer(configName:String,timezoneConverters:List[Either[String,Tuple2[String,String]]] = List(Left("EEE MMM dd kk:mm:ss z yyyy"))) extends GenericXmlSerializer(configName) with Logger {

  override def toMeTLImage(input:NodeSeq):MeTLImage = {
    val m = parseMeTLContent(input,config)
    val c = parseCanvasContent(input)
    val tag = getStringByName(input,"tag")
    val imageBytes = base64Decode(getStringByName(input,"imageBytes"))
    val newUrl = config.postResource(c.slide.toString,nextFuncName,imageBytes)
    val source = Full(newUrl)
    val pngBytes = Empty
    val width = getDoubleByName(input,"width")
    val height = getDoubleByName(input,"height")
    val x = getDoubleByName(input,"x")
    val y = getDoubleByName(input,"y")
    MeTLImage(config,m.author,m.timestamp,tag,source,Full(imageBytes),pngBytes,width,height,x,y,c.target,c.privacy,c.slide,c.identity,m.audiences)
  }
  val dateTimeFormatter = new com.metl.MultiFormatDateFormatter(timezoneConverters.map(_.right.map(lt => (lt._1,ZoneId.of(lt._2)))):_*)
    override def fromConversation(input:Conversation):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromConversation",{
      val created:Long = dateTimeFormatter.parse(input.created)
      metlXmlToXml("conversation",List(
        <author>{input.author}</author>,
        <lastAccessed>{input.lastAccessed}</lastAccessed>,
        <slides>{input.slides.map(s => fromSlide(s))}</slides>,
        <subject>{input.subject}</subject>,
        <tag>{input.tag}</tag>,
        <jid>{input.jid}</jid>,
        <title>{input.title}</title>,
        <created>{new java.util.Date(created).toString()}</created>,
        <creation>{created}</creation>,
        <blacklist>{
          input.blackList.map(bu => <user>{bu}</user> )
          }</blacklist>,
        fromPermissions(input.permissions)
      ))
    })
  override def fromMeTLImage(input:MeTLImage):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLImage",{
    canvasContentToXml("image",input,List(
      <tag>{input.tag}</tag>,
      <imageBytes>{base64Encode(input.imageBytes.getOrElse(Array.empty[Byte]))}</imageBytes>,
      <width>{input.width}</width>,
      <height>{input.height}</height>,
      <x>{input.x}</x>,
      <y>{input.y}</y>
    ))
  })
  override def toMeTLQuiz(input:NodeSeq):MeTLQuiz = Stopwatch.time("GenericXmlSerializer.toMeTLQuiz", {
    val m = parseMeTLContent(input,config)
    val created = getLongByName(input,"created")
    val question = getStringByName(input,"question") match {
      case q if (q.length > 0) => q
      case _ => getStringByName(input,"title")
    }
    val id = getStringByName(input,"id")
    val quizImage = Full(base64Decode(getStringByName(input,"imageBytes")))
    val newUrl = quizImage.map(qi => config.postResource("quizImages",nextFuncName,qi))
    val isDeleted = getBooleanByName(input,"isDeleted")
    val options = getXmlByName(input,"quizOption").map(qo => toQuizOption(qo)).toList
    MeTLQuiz(config,m.author,m.timestamp,created,question,id,newUrl,quizImage,isDeleted,options,m.audiences)
  })
  override def fromMeTLQuiz(input:MeTLQuiz):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLQuiz", {
    metlContentToXml("quiz",input,List(
      <created>{input.created}</created>,
      <question>{input.question}</question>,
      <id>{input.id}</id>,
      <isDeleted>{input.isDeleted}</isDeleted>,
      <options>{input.options.map(o => fromQuizOption(o))}</options>
    ) ::: input.imageBytes.map(ib => List(<imageBytes>{base64Encode(ib)}</imageBytes>)).openOr(List.empty[Node]))
  })
  override def toSubmission(input:NodeSeq):MeTLSubmission = Stopwatch.time("GenericXmlSerializer.toSubmission", {
    val m = parseMeTLContent(input,config)
    val c = parseCanvasContent(input)
    val title = getStringByName(input,"title")
    val imageBytes = Full(base64Decode(getStringByName(input,"imageBytes")))
    val url = imageBytes.map(ib => config.postResource(c.slide.toString,nextFuncName,ib)).getOrElse("unknown")
    val blacklist = getXmlByName(input,"blacklist").map(bl => {
      val username = getStringByName(bl,"username")
      val highlight = getColorByName(bl,"highlight")
      SubmissionBlacklistedPerson(username,highlight)
    }).toList
    MeTLSubmission(config,m.author,m.timestamp,title,c.slide.toInt,url,imageBytes,blacklist,c.target,c.privacy,c.identity,m.audiences)
  })
  override def fromSubmission(input:MeTLSubmission):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromSubmission", {
    canvasContentToXml("screenshotSubmission",input,List(
      <imageBytes>{base64Encode(input.imageBytes.getOrElse(Array.empty[Byte]))}</imageBytes>,
      <title>{input.title}</title>,
      <time>{input.timestamp.toString}</time>
    ) ::: input.blacklist.map(bl => <blacklist><username>{bl.username}</username><highlight>{ColorConverter.toRGBAString(bl.highlight)}</highlight></blacklist> ).toList)
  })
  override def toMeTLFile(input:NodeSeq):MeTLFile = Stopwatch.time("GenericXmlSerializer.toMeTLFile",{
    val m = parseMeTLContent(input,config)
    val name = getStringByName(input,"name")
    val id = getStringByName(input,"id")
    val bytes = Full(base64Decode(getStringByName(input,"bytes")))
    val url = bytes.map(ib => config.postResource("files",nextFuncName,ib))
    MeTLFile(config,m.author,m.timestamp,name,id,url,bytes)
  })
  override def fromMeTLFile(input:MeTLFile):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLFile",{
    metlContentToXml("file",input,List(
      <name>{input.name}</name>,
      <id>{input.id}</id>
    ) :::
      input.bytes.map(ib => List(<bytes>{base64Encode(ib)}</bytes>)).getOrElse(List.empty[Node]))
  })
}

object ReadOnlyMeTL2011ZipAdaptorConfigurator extends ServerConfigurator{
  override def matchFunction(e:Node) = (e \\ "type").headOption.exists(_.text == "MeTL2011Zip")
  override def interpret(e:Node,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentailsFunc:()=>Tuple2[String,String],conversationListenerCredentialsFunc:()=>Tuple2[String,String],httpCredentialsFunc:()=>Tuple2[String,String]) = {
    for {
      name <- (e \\ "name").headOption.map(_.text)
      historyZipPath <- (e \\ "historyZipPath").headOption.map(_.text)
      resourceZipPath <- (e \\ "resourceZipPath").headOption.map(_.text)
      structureZipPath <- (e \\ "structureZipPath").headOption.map(_.text)
    } yield {
      new ReadOnlyMeTL2011ZipAdaptor(name,historyZipPath,structureZipPath,resourceZipPath)
    }
  }
}

object MeTL2011Util {
  /** Convert MeTL2011 timestamp (nanos since 1/1/0000) to Java timestamp (millis since 1/1/1970). */
  def fromTicks(ticks: String):Long = {
    val ticksPerMilli = 10000
    val ticksInMilli = BigInt(ticks) / ticksPerMilli
    val calendar = new GregorianCalendar()
    // Set to C# start time: 1 January 0000
    calendar.set(1,0,0)
    val epochInMilli = calendar.getTimeInMillis * -1
    new Date((ticksInMilli - epochInMilli).toLong).getTime
  }
}
class ReadOnlyMeTL2011ZipAdaptor(name:String,historyZipPath:String,structureZipPath:String,resourcesZipPath:String) extends ServerConfiguration(name,"_unused",(c:Conversation) => {}) with Logger {
  import java.io._
  import java.util.zip.{ZipInputStream,ZipEntry}
  import org.apache.commons.io.IOUtils
  protected val thisConfig:ServerConfiguration = this
  // these are all irrelevant
  override def getMessageBus(d:MessageBusDefinition):MessageBus = EmptyMessageBus
  override def createConversation(title:String,author:String):Conversation = Conversation.empty
  override def deleteConversation(jid:String):Conversation = Conversation.empty
  override def renameConversation(jid:String,newTitle:String):Conversation = Conversation.empty
  override def changePermissions(jid:String,newPermissions:Permissions):Conversation = Conversation.empty
  override def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = Conversation.empty
  override def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = Conversation.empty
  override def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = Conversation.empty
  override def updateConversation(jid:String,newConversation:Conversation):Conversation = Conversation.empty
  override def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = ""
  override def insertResource(jid:String,data:Array[Byte]):String = ""
  override def upsertResource(jid:String,identifier:String,data:Array[Byte]):String = ""

  //shutdown is a function to be called when the serverConfiguration is to be disposed
  override def shutdown:Unit = {}
  override def isReady:Boolean = {
    loadStructure
    debug("found conversations: %s".format(conversationCache.keys.toList))
    loadResources
    debug("found resources: %s".format(resourceCache.keys.toList))
    loadHistories
    debug("found histories: %s".format(historyCache.keys.toList))
    true
  }

  var conversationCache = Map.empty[Int,Conversation]
  var historyCache = Map.empty[String,History]
  var resourceCache = Map.empty[String,Map[String,Array[Byte]]]

  protected val serializer = new MeTL2011XmlSerializer(name,true) {
    protected val utils = new MeTL2011Utils(name)
    protected def safetyPath(in:String,prefix:String = "Resource"):Box[String] = {
      try {
        (in match {
          case s:String if (s.length > 0 && s != "unknown url" && s != "none") => utils.reabsolutizeUri(s,prefix)
          case _ => Empty
        }).flatMap(ap => utils.deabsolutizeUri(ap,thisConfig))
      } catch {
        case e:Exception => {
          warn("failed to safety, so returning original string: %s".format(in))
          Full(in)
          //Failure("failure to safety",Full(e),Empty)
        }
      }
    }
    override def toMeTLImage(input:NodeSeq):MeTLImage = Stopwatch.time("MeTL2011XmlSerializer.toMeTLImage",{
      trace("deserializing image from xmpp: %s".format(input))
      val m = parseMeTLContent(input)
      val c = parseCanvasContent(input)
      val tag = getStringByName(input,"tag")
      val source = safetyPath(getStringByName(input,"source"))
      val imageBytes = source.map(u => thisConfig.getResource(u))
      val pngBytes = Empty
      val width = getDoubleByName(input,"width")
      val height = getDoubleByName(input,"height")
      val x = getDoubleByName(input,"x")
      val y = getDoubleByName(input,"y")
      MeTLImage(config,m.author,m.timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,c.target,c.privacy,c.slide,c.identity,m.audiences)
    })
    override def toMeTLCommand(input:NodeSeq):MeTLCommand = Stopwatch.time("MeTL2011XmlSerializer.toMeTLCommand",{
      val m = parseMeTLContent(input)
      val body = input match {
        case t:Text => t.toString.split(" ")
        case e:Elem => getStringByName(e,"body").split(" ")
        case other => other.toString.split(" ")
      }
      val comm = body.head
      val parameters = body.tail.toList
      MeTLCommand(config,m.author,m.timestamp,comm,parameters,m.audiences)
    })
    override def toSubmission(input:NodeSeq):MeTLSubmission = Stopwatch.time("GenericXmlSerializer.toSubmission",{
      trace("submission attempted: %s".format(input))
      val m = parseMeTLContent(input)
      val c = parseCanvasContent(input)
      val title = getStringByName(input,"title")
      val urlBox = safetyPath(getStringByName(input,"url"))
      val imageBytes = urlBox.map(url => thisConfig.getResource(url))
      val url = urlBox.openOr("no valid url specified")
      val blacklist = getXmlByName(input,"blacklist").map(bl => {
        val username = getStringByName(bl,"username")
        val highlight = getColorByName(bl,"highlight")
        SubmissionBlacklistedPerson(username,highlight)
      }).toList
      MeTLSubmission(config,m.author,m.timestamp,title,c.slide.toInt,url,imageBytes,blacklist,c.target,c.privacy,c.identity,m.audiences)
    })
    override def toMeTLQuiz(input:NodeSeq):MeTLQuiz = Stopwatch.time("MeTL2011XmlSerializer.toMeTLQuiz",{
      trace("quiz attempted: %s".format(input))
      val m = parseMeTLContent(input)
      try {
        val created = getStringByName(input,"created") match {
          case c:String if c.length >= 17 => MeTL2011Util.fromTicks(c)
          case _ => getLongByName(input,"created")
        }
        val question = getStringByName(input,"question") match {
          case q if (q.length > 0) => q
          case _ => getStringByName(input,"title")
        }
        val id = getStringByName(input,"id")
        val url = safetyPath(getStringByName(input,"url"))
        val quizImage = url.map(u => thisConfig.getResource(u))
        val isDeleted = getBooleanByName(input,"isDeleted")
        val options = getXmlByName(input,"quizOption").map(qo => toQuizOption(qo)).toList
        MeTLQuiz(config,m.author,m.timestamp,created,question,id,url,quizImage,isDeleted,options,m.audiences)
      } catch {
        case e:Throwable => {
          error("failed to construct MeTLQuiz",e)
          MeTLQuiz(config,m.author,m.timestamp,0L,"","",Empty,Empty,true,List.empty[QuizOption],m.audiences)	
        }
      }
    })
    override def toMeTLFile(input:NodeSeq):MeTLFile = Stopwatch.time("MeTL2011XmlSerializer.toMeTLFile",{
      trace("file attempted: %s".format(input))
      val m = parseMeTLContent(input)
      try {
        val name = getStringByName(input,"name")
        // If ID is blank (eg MeTL2011 from C#) then use time instead.
        val id = Some(getStringByName(input,"id")).filter(_.length > 0).getOrElse(getStringByName(input,"time"))
        val url = safetyPath(getStringByName(input,"url"))
        val bytes = url.map(u => thisConfig.getResource(u))
        MeTLFile(config, m.author, m.timestamp, name, id, url, bytes, m.audiences)
      } catch {
        case e:Throwable => {
          error("failed to construct MeTLFile",e)
          MeTLFile.empty
        }
      }
    })
    override def toSlide(input:NodeSeq):Slide = Stopwatch.time("MeTL2011XmlSerializer.toSlide",{
      val m = parseMeTLContent(input,config)
      val author = getStringByName(input,"author")
      val id = getIntByName(input,"id")
      val index = getIntByName(input,"index")
      val defHeight = getIntByName(input,"defaultHeight")
      val defWidth = getIntByName(input,"defaultWidth")
      // Always treat imported slides as "exposed".
      val exposed = true
      val slideType = getStringByName(input,"type")
      val groupSets = (input \ "groupSet").map(gs => toGroupSet(gs)).toList
      Slide(config,author,id,index,defHeight,defWidth,exposed,slideType,groupSets,m.audiences)
    })
  }

  var attendancesInPrivateRooms:Map[String,List[String]] = Map.empty[String,List[String]]

  def getPrivateAuthorsForConversation(conversationJid:String):List[String] = attendancesInPrivateRooms.get(conversationJid).getOrElse(Nil)
  protected def isStem(source:String,stem:String):Boolean = {
    "00000%s".format(source).reverse.drop(3).take(2).reverse.mkString("") == stem
  }
  protected def inStemmedJid(prefix:Option[String],ze:ZipEntry,func:Tuple3[String,String,ZipEntry] => Unit):Unit = {
    val rawName = ze.getName
    rawName.split('/').toList match {
      case _ if ze.isDirectory() => {} // do nothing, it's a directory
      case List(p,stem,jid,filename) if isStem(jid,stem) && prefix.contains(p) => func(jid,filename,ze)
      case List(stem,jid,filename) if isStem(jid,stem) => func(jid,filename,ze)
      case List(p,stem,user,jid,filename) if isStem(user,stem) && prefix.contains(p) => {
        attendancesInPrivateRooms = attendancesInPrivateRooms.updated(jid,(user :: attendancesInPrivateRooms.get(jid).getOrElse(Nil)).distinct)
        func("%s%s".format(jid,user),filename,ze)
      }
      case List(stem,user,jid,filename) if isStem(user,stem) => {
        attendancesInPrivateRooms = attendancesInPrivateRooms.updated(jid,(user :: attendancesInPrivateRooms.get(jid).getOrElse(Nil)).distinct)
        func("%s%s".format(jid,user),filename,ze)
      }
      case other => {
        warn("other found inStemmedJid: %s".format(other))
      }
    }
  }
  protected def loadStructure:Unit = {
    inZipFile(structureZipPath,{
      case (inputStream:ZipInputStream,ze:ZipEntry) if ze.getName.endsWith(".xml") => {
        try {
          val xml = scala.xml.XML.loadString(IOUtils.toString(inputStream))
          val conv = serializer.toConversation(xml)
          conversationCache = conversationCache.updated(conv.jid,conv)
        } catch {
          case e:Exception => {
            error("exception parsing xml as conversation: %s".format(ze.getName),e)  
          }
        }
      }
      case _ => {}
    })
  }
  protected def loadResources:Unit = {
    inZipFile(resourcesZipPath,{
      case (inputStream:ZipInputStream,oze:ZipEntry) => {
        inStemmedJid(Some("Resource"),oze,{
          case (jid:String,filename:String,zipEntry:ZipEntry) => {
            val perJidMap = resourceCache.get(jid).getOrElse(Map.empty[String,Array[Byte]])
            resourceCache = resourceCache.updated(jid,perJidMap.updated(filename,IOUtils.toByteArray(inputStream)))
          }
          case _ => {}
        })
      }
      case _ => {}
    })
    trace("resource cache: %s".format(resourceCache))
  }
  protected def loadHistories:Unit = {
    inZipFile(historyZipPath,{
      case (inputStream:ZipInputStream,oze:ZipEntry) => {
        inStemmedJid(Some("Structure"),oze,{
          case (jid:String,filename:String,zipEntry:ZipEntry) => {
            if (filename.endsWith(".xml")) {
              val xmlString = IOUtils.toString(inputStream) + "</logCollection>"
              val xml = scala.xml.XML.loadString(xmlString)
              val history = historyCache.get(jid).getOrElse(new History(jid))
              (xml \\ "message").map(serializer.toMeTLData _).map{
                case m:MeTLUnhandledData => try {
                  history.addStanza(serializer.toMeTLCommand(scala.xml.XML.loadString(m.unhandled)))
                } catch {
                  case e:Exception => {
                    warn("found unhandled metldata in history: %s".format(m))
                  }
                }
                case s:MeTLStanza => history.addStanza(s)
                case other => {
                  warn("found unknown metldata in history: %s".format(other))
                }
              }
              historyCache = historyCache.updated(jid,history)
              //info("found history xml file: %s || %s || %s || %s".format(jid,filename,history,history.getAll.length))
            }
          }
        })
      }
      case _ => {}
    })
  }

  protected def inZipFile(zipPath:String,perFile:Tuple2[ZipInputStream,ZipEntry]=>Unit):Unit = {
		try {
      val stream = new FileInputStream(new File(zipPath))
      val zipStream = new java.util.zip.ZipInputStream(stream)
      var ze:ZipEntry = zipStream.getNextEntry
      while (ze != null){
        perFile(zipStream,ze)
        //trace("parsing: %s".format(ze.getName()))
        ze = zipStream.getNextEntry
      }
      zipStream.close
		} catch {
			case e:Throwable => {
        error("exception during zipUnpacking:",e)
      }
		}
  }


  // only these matter
  override def getHistory(jid:String):History = historyCache.get(jid).getOrElse(History.empty)
  override def getConversationForSlide(slideJid:String):String = (slideJid.toInt % 1000).toString
  override def searchForConversation(query:String):List[Conversation] = conversationCache.values.toList.filter(c => c.title.toLowerCase.trim.contains(query.toLowerCase.trim) || c.author.toLowerCase.trim == query)
  override def detailsOfConversation(jid:String):Conversation = conversationCache.get(jid.toInt).getOrElse(Conversation.empty)
  override def getImage(jid:String,identity:String):MeTLImage = getHistory(jid).getImageByIdentity(identity).getOrElse(MeTLImage.empty)
  override def getResource(jid:String,identifier:String):Array[Byte] = resourceCache.get(jid).flatMap(_.get(identifier)).getOrElse({
    warn("failed to find resource: %s %s".format(jid,identifier))
    Array.empty[Byte]
  })
  override def getImage(identity:String):MeTLImage = getImage(commonLocation,identity)
  override def getResource(identifier:String):Array[Byte] = identifier.split('/').toList.filterNot(_.trim == "") match {
    case List("Resource",stem,jid,filename) => getResource(jid,filename)
    case other => {
      error("unknown file pattern for resource: %s => %s".format(identifier,other))
      Array.empty[Byte]//getResource(commonLocation,identifier)
    }
  }
}
