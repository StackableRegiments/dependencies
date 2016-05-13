package com.metl.datamigrator
import com.metl.data._
import com.metl.metl2011._
import com.metl.utils.{Http => MeTLHttp,_}
import com.metl.h2._
import net.liftweb.common._
import net.liftweb.actor._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import scala.xml._
import dispatch._
import Defaults._

object Application extends Logger {
  case class AddKey(h:String,k:String,v:String)
  case class RemoveKey(h:String,k:String,v:String)
  class MaintainKeys(start:Long) extends LiftActor {
    case object Ping
    var keys:List[Tuple3[String,String,String]] = Nil
    def repeat = {
      if (keys.length > 0){
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
        keys.foreach(k => {
          val svc = url("%s/authenticationState".format(k._1)).GET <:< Map("Cookie" -> "%s=%s".format(k._2,k._3))
          val result = Http(svc OK as.String).either.right.map(r => (XML.loadString(r) \\ "@id").exists(_.text.toString == "authData"))
          info("[%sms] maintainingKey: %s".format(new java.util.Date().getTime - start, result()))
        })
        repeat
      }
    }
  }

  def main(args:Array[String]):Unit = {
    val start = new java.util.Date().getTime
    val maintainKeys = new MaintainKeys(start)
    def mark(msg:String) = {
      info("[%sms] %s".format(new java.util.Date().getTime - start,msg))
    }
    val configurationFileLocation = System.getProperty("metlx.configurationFile")
    MeTL2011ServerConfiguration.initialize
    MeTL2015ServerConfiguration.initialize
    LocalH2ServerConfiguration.initialize
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
    val sortConversations:List[Conversation]=>List[Conversation] = {
      val priorityElems = (configFile \\ "priority")
      val jidPriorities = priorityElems.flatMap(elem => (elem \ "@jid").headOption.map(_.text.toInt))
      val authorPriorities = priorityElems.flatMap(elem => (elem \ "@author").headOption.map(_.text))
      (in:List[Conversation]) => {
        val (jidMatches,nonJid) = in.sortWith((a,b) => a.lastAccessed > b.lastAccessed).partition(c => jidPriorities.contains(c.jid))
        val (authorMatches,nonAuthor) = nonJid.partition(c => authorPriorities.contains(c.author))
        jidMatches ::: authorMatches ::: nonAuthor
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
      var exportSerializer = new MigratorXmlSerializer(config.name)
      def exportConversation(onBehalfOfUser:String,conversation:String):Box[NodeSeq] = {
        for (
          conv <- Some(config.detailsOfConversation(conversation));
          if (onBehalfOfUser == conv.author);
          histories = exportHistories(conv,None);
          xml = {
            <export>
            {exportSerializer.fromConversation(conv)}
            <histories>{histories.map(h => exportSerializer.fromHistory(h))}</histories>
            </export>
          }
        ) yield {
          xml  
        }
      }
      def time[A](label:String,action: => A):A = {
        val s = new java.util.Date().getTime
        val res = action
        info(label + "(%s ms)".format(new java.util.Date().getTime - s))
        res
      }
      def exportHistories(conversation:Conversation,restrictToPrivateUsers:Option[List[String]]):List[History] = {
        val convHistory = time("getHistory(%s)".format(conversation.jid),config.getHistory(conversation.jid.toString).filter(m => {
          restrictToPrivateUsers.map(users => {
            m match {
              case q:MeTLQuiz => true
              case mcc:MeTLCanvasContent => mcc.privacy == Privacy.PUBLIC || users.contains(mcc.author)
              case ms:MeTLStanza => users.contains(ms.author)
            }
          }).getOrElse(true)
        }))
        val participants = (convHistory.getAttendances.map(_.author) ::: restrictToPrivateUsers.getOrElse(List.empty[String])).distinct.filter(u => restrictToPrivateUsers.map(_.contains(u)).getOrElse(true))
        val histories = convHistory :: conversation.slides.flatMap(slide => {
          val slideJid = slide.id.toString
          val publicHistory = /*time("getPublicSlideHistory(%s)".format(slideJid),*/config.getHistory(slideJid)/*)*/
          val privateHistories = participants.map(p => /*time("getPrivateSlideHistory(%s)".format(slideJid + p),*/config.getHistory(slideJid + p)/*)*/)
          publicHistory :: privateHistories
        })
        histories
      }
      val rawConvs = config.searchForConversation("")//.take(25) // hopefully every conversation will be returned by this query?  Will probably have to write an explicit "getAllConversations" call into the backends.
      val convs = sortConversations(rawConvs)
      mark("fetched conversations: %s".format(convs.length))
      val p = convs.par
      parallelism.map(paraCount => {
        p.tasksupport = new scala.collection.parallel.ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(paraCount.getOrElse(1)))
      })
      val completed = p.flatMap(conversation => {
        val es = new java.util.Date().getTime  
        time("exporting conversation",exportConversation(conversation.author,conversation.jid.toString)).map(xml => {
          val ee = new java.util.Date().getTime  
          mark("exported conversation: %s (%s) %s".format(conversation.author, conversation.slides.length, conversation.title))
          val is = new java.util.Date().getTime  
          val svc = url("%s/conversationImport".format(targetServer)).POST << xml.toString <:< Map("Cookie" -> "%s=%s".format(cookieKey,cookieValue))
          val result = Http(svc OK as.xml.Elem).either
          val res = result()
          //val res = Right(xml \\ "conversation" \\ "jid") // skipping the upload while I work on optimizing the import speed
          val ie = new java.util.Date().getTime
          mark("pushed conversation: %s (%s) %s => %s (%sB)".format(conversation.author,conversation.slides.length,conversation.title,res.isRight,xml.toString.length))
          (conversation.jid,res,ee - es, ie - is, ie - es)
        })
      }).toList  
      mark("completed pushing conversations: \r\n%s".format(completed.map(c => "CONV %s [%s (%s + %s)]: %s".format(c._1,c._5,c._3,c._4,c._2)).mkString("\r\n")))
      maintainKeys ! RemoveKey(targetServer,cookieKey,cookieValue)
    })
    LAScheduler.shutdown()
    mark("finished reading.")
    System.exit(0)
  }
}

class MigratorXmlSerializer(configName:String) extends GenericXmlSerializer(configName) with Logger {
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
