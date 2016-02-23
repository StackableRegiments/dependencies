package com.metl.datamigrator
import com.metl.data._
import com.metl.metl2011._
import com.metl.utils._
import com.metl.h2._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import scala.xml._

object Application {
  def main(args:Array[String]):Unit = {
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
    val targetServer = (XML.load(configurationFileLocation) \\ "targetServer").headOption.map(_.text).getOrElse({throw new Exception("please specify a target server baseUrl in the configuration file")})
    println("servers: %s => %s".format(servers,targetServer))
    servers.filterNot(_ == EmptyBackendAdaptor).foreach(config => {
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
      def exportHistories(conversation:Conversation,restrictToPrivateUsers:Option[List[String]]):List[History] = {
        val convHistory = config.getHistory(conversation.jid.toString).filter(m => {
          restrictToPrivateUsers.map(users => {
            m match {
              case q:MeTLQuiz => true
              case mcc:MeTLCanvasContent => mcc.privacy == Privacy.PUBLIC || users.contains(mcc.author)
              case ms:MeTLStanza => users.contains(ms.author)
            }
          }).getOrElse(true)
        })
        val participants = (convHistory.getAttendances.map(_.author) ::: restrictToPrivateUsers.getOrElse(List.empty[String])).distinct.filter(u => restrictToPrivateUsers.map(_.contains(u)).getOrElse(true))
        val histories = convHistory :: conversation.slides.flatMap(slide => {
          val slideJid = slide.id.toString
          val publicHistory = config.getHistory(slideJid)
          val privateHistories = participants.map(p => config.getHistory(slideJid + p))
          publicHistory :: privateHistories
        })
        histories
      }
      println("loading server: %s".format(config))
      config.searchForConversation("").foreach(conversation => {
        val xml = exportConversation(conversation.author,conversation.jid.toString)
        println("exporting conversation: %s".format(xml.toString.take(80)))
      }) // hopefully every conversation will be returned by this query?
    })
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
