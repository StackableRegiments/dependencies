package com.metl.datamigrator

import com.metl.data._
import com.metl.metl2011.{MeTL2011Utils, MeTL2011XmlSerializer}
import com.metl.utils.Stopwatch
import net.liftweb.common.{Box, Empty, Full, Logger}

import scala.xml.{Elem, Node, NodeSeq, Text}

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

class ReadOnlyMeTL2011ZipAdaptor(name:String,historyZipPath:String,structureZipPath:String,resourcesZipPath:String)
  extends ServerConfiguration(name,"_unused",(c:Conversation) => {}) with Logger {
  import java.io._
  import java.util.zip.{ZipEntry, ZipInputStream}

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

