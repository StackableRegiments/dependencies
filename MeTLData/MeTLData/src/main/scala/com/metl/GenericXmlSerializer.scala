package com.metl.model

import scala.xml._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import Privacy._

object XmlUtils {
	def getPrivacyByName(content:NodeSeq,name:String):Privacy = tryo(Privacy.parse((content \\ name).text)).openOr(Privacy.PUBLIC)
	def getColorByName(content:NodeSeq,name:String):Color = tryo(ColorConverter.fromText(getValueOfNode(content,name))).openOr(Color.default)
	def getStringByName(content:NodeSeq,name:String):String = tryo(getValueOfNode(content,name)).openOr("unknown "+name)
	def getBooleanByName(content:NodeSeq,name:String):Boolean = tryo(getValueOfNode(content,name).toBoolean).openOr(false)
	def getDoubleByName(content:NodeSeq,name:String):Double = tryo(getValueOfNode(content,name).toDouble).openOr(-1.0)
	def getLongByName(content:NodeSeq,name:String):Long = tryo(getValueOfNode(content,name).toLong).openOr(-1L)
	def getIntByName(content:NodeSeq,name:String):Int = tryo(getValueOfNode(content,name).toInt).openOr(-1)
	def getListOfStringsByNameWithin(content:NodeSeq,name:String,containerName:String) = tryo(getXmlByName(content,containerName).map(i => getStringByName(i,name)).toList).openOr(List.empty[String])
	def getValueOfNode(content:NodeSeq,nodeName:String):String = tryo((content \\ nodeName).text).openOr("")
	def getXmlByName(content:NodeSeq,name:String):NodeSeq = tryo((content \\ name)).openOr(NodeSeq.Empty)
	def getAttributeOfNode(content:NodeSeq,nodeName:String,attributeName:String):String = tryo((content \\ nodeName).seq(0).attribute(attributeName).getOrElse(NodeSeq.Empty).text).openOr("")
	def parseCanvasContent(i:NodeSeq):ParsedCanvasContent = {
		val target = getStringByName(i,"target")
		val privacy = getPrivacyByName(i,"privacy")
		val slide = getStringByName(i,"slide")
		val identity = getStringByName(i,"identity")
		ParsedCanvasContent(target,privacy,slide,identity)	
	}
	def parsedCanvasContentToXml(p:ParsedCanvasContent):List[NodeSeq] = {
		List(
			<target>{p.target}</target>,
			<privacy>{p.privacy.toString.toLowerCase}</privacy>,
			<slide>{p.slide}</slide>,
			<identity>{p.identity}</identity>
		)	
	}
	def parseMeTLContent(i:NodeSeq):ParsedMeTLContent = {
		val author = getStringByName(i,"author")
		val timestamp = tryo(getAttributeOfNode(i,"message","time").toLong).openOr(-1L)
		ParsedMeTLContent(author,timestamp)
	}
	def parsedMeTLContentToXml(p:ParsedMeTLContent):List[NodeSeq] = {
		List(
			<author>{p.author}</author>
		)
	}
}
case class ParsedMeTLContent(author:String,timestamp:Long)
case class ParsedCanvasContent(target:String,privacy:Privacy,slide:String,identity:String)

class GenericXmlSerializer(configName:String) extends Serializer{
	type T = NodeSeq
	lazy val config = ServerConfiguration.configForName(configName)
	val utils = XmlUtils
	override def toMeTLStanza(input:NodeSeq):MeTLStanza = Stopwatch.time("GenericXmlSerializer.toMeTLStanza", () => {
		input match {
			case i:NodeSeq if ((i \ "ink").length > 0) => toMeTLInk(i)
			case i:NodeSeq if ((i \ "textbox").length > 0) => toMeTLText(i)
			case i:NodeSeq if ((i \ "image").length > 0) => toMeTLImage(i)
			case i:NodeSeq if ((i \ "dirtyInk").length > 0) => toMeTLDirtyInk(i)
			case i:NodeSeq if ((i \ "dirtyText").length > 0) => toMeTLDirtyText(i)
			case i:NodeSeq if ((i \ "dirtyImage").length > 0) => toMeTLDirtyImage(i)
			case i:NodeSeq if ((i \ "moveDelta").length > 0) => toMeTLMoveDelta(i)
			case i:NodeSeq if ((i \ "quiz").length > 0) => toMeTLQuiz(i)
			case i:NodeSeq if ((i \ "quizResponse").length > 0) => toMeTLQuizResponse(i)
			case i:NodeSeq if ((i \ "screenshotSubmission").length > 0) => toSubmission(i)
			case i:NodeSeq if ((i \ "body").length > 0) => toMeTLCommand(i)
			case i:NodeSeq if (((i \\ "author").length > 0) && ((i \\ "message").length > 0)) => {
				val m = utils.parseMeTLContent(i)
				MeTLStanza(config,m.author,m.timestamp)
			}
		}
	})
	private def metlXmlToXml(rootName:String,additionalNodes:List[NodeSeq],wrapWithMessage:Boolean = false,additionalAttributes:List[(String,String)] = List.empty[(String,String)]) = Stopwatch.time("GenericXmlSerializer.metlXmlToXml", () => {
		val attrs = additionalAttributes.foldLeft(scala.xml.Null.asInstanceOf[scala.xml.MetaData])((acc,item) => {
			item match {
				case (k:String,v:String) => new UnprefixedAttribute(k,v,acc)
				case _ => acc
			}	
		})
		wrapWithMessage match {
			case true => {
				Elem(null, "message", attrs, TopScope, Elem(null, rootName, null, TopScope, Group(additionalNodes.asInstanceOf[scala.collection.Seq[Node]])))
			}
			case _ => Elem(null, rootName, attrs, TopScope, Group(additionalNodes.asInstanceOf[scala.collection.Seq[Node]]))
		}
	})
	private def metlContentToXml(rootName:String,input:MeTLStanza,additionalNodes:List[NodeSeq]) = Stopwatch.time("GenericXmlSerializer.metlContentToXml", () => {
		val pmc = utils.parsedMeTLContentToXml(ParsedMeTLContent(input.author,input.timestamp)) ::: additionalNodes
		metlXmlToXml(rootName,pmc,true,List(("timestamp",input.timestamp.toString)))
	})
	private def canvasContentToXml(rootName:String,input:MeTLCanvasContent,additionalNodes:List[NodeSeq]) = Stopwatch.time("GenericXmlSerializer.canvasContentToXml", () => {
		metlContentToXml(rootName,input,utils.parsedCanvasContentToXml(ParsedCanvasContent(input.target,input.privacy,input.slide,input.identity)) ::: additionalNodes)
	})
	override def fromHistory(input:History):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromHistory", () => {
		<history>{input.getAll.map(i => fromMeTLStanza(i))}</history>
	})
	override def toMeTLMoveDelta(input:NodeSeq):MeTLMoveDelta = Stopwatch.time("GenericXmlSerializer.toMeTLMoveDelta", () => {
		val m = utils.parseMeTLContent(input)
		val c = utils.parseCanvasContent(input)
		val inkIds = utils.getListOfStringsByNameWithin(input,"identity","inkIds")
		val textIds = utils.getListOfStringsByNameWithin(input,"identity","textIds")
		val imageIds = utils.getListOfStringsByNameWithin(input,"identity","imageIds")
    val xTranslate = utils.getDoubleByName(input,"xTranslate")
    val yTranslate = utils.getDoubleByName(input,"yTranslate")
		val xScale = utils.getDoubleByName(input,"xScale")
		val yScale = utils.getDoubleByName(input,"yScale")
		MeTLMoveDelta(config,m.author,m.timestamp,c.target,c.privacy,c.slide,"moveDelta",inkIds,textIds,imageIds,xTranslate,yTranslate,xScale,yScale)
	})
	override def fromMeTLMoveDelta(input:MeTLMoveDelta):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLMoveDelta", () => {
		canvasContentToXml("moveDelta",input, List(
			<inkIds>{input.inkIds.map(i => <identity>{i}</identity>)}</inkIds>,
			<imageIds>{input.imageIds.map(i => <identity>{i}</identity>)}</imageIds>,
			<textIds>{input.textIds.map(i => <identity>{i}</identity>)}</textIds>,
			<xTranslate>{input.xTranslate}</xTranslate>,
			<yTranslate>{input.yTranslate}</yTranslate>,
			<xScale>{input.xScale}</xScale>,
			<yScale>{input.yScale}</yScale>
		))
	})
	override def toMeTLInk(input:NodeSeq):MeTLInk = Stopwatch.time("GenericXmlSerializer.toMeTLImage", () => {
		val m = utils.parseMeTLContent(input)
		val c = utils.parseCanvasContent(input)
		val checksum = utils.getDoubleByName(input,"checksum")
		val startingSum = utils.getDoubleByName(input,"startingSum")
		val points = tryo(PointConverter.fromText(utils.getStringByName(input,"points"))).openOr(List.empty[Point])
		val color = utils.getColorByName(input,"color")
		val thickness = utils.getDoubleByName(input,"thickness")
		val isHighlighter = utils.getBooleanByName(input,"highlight")
		val identity = startingSum.toString
		MeTLInk(config,m.author,m.timestamp,checksum,startingSum,points,color,thickness,isHighlighter,c.target,c.privacy,c.slide,identity)
	})
	override def fromMeTLInk(input:MeTLInk):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLInk",() => {
		canvasContentToXml("ink",input,List(
			<checksum>{input.checksum}</checksum>,
			<startingSum>{input.startingSum}</startingSum>,
			<points>{PointConverter.toText(input.points)}</points>,
			<color>{ColorConverter.toRGBAString(input.color)}</color>,
			<thickness>{input.thickness}</thickness>,
			<highlight>{input.isHighlighter}</highlight>
		))
	})
	override def toMeTLImage(input:NodeSeq):MeTLImage = Stopwatch.time("GenericXmlSerializer.toMeTLImage",() => {
		val m = utils.parseMeTLContent(input)
		val c = utils.parseCanvasContent(input)
		val tag = utils.getStringByName(input,"tag")
		val source = utils.getStringByName(input,"source") match {
			case s:String if (s.length > 0 && s != "unknown url" && s != "none") => Full(s) 
			case _ => Empty
		}
		val imageBytes = source.map(u => config.getResource(u))
		val pngBytes = Empty
		val width = utils.getDoubleByName(input,"width")
		val height = utils.getDoubleByName(input,"height")
		val x = utils.getDoubleByName(input,"x")
		val y = utils.getDoubleByName(input,"y")
		MeTLImage(config,m.author,m.timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,c.target,c.privacy,c.slide,c.identity)
	})
	override def fromMeTLImage(input:MeTLImage):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLImage",() => {
		canvasContentToXml("image",input,List(
			<tag>{input.tag}</tag>,
			<source>{input.source.openOr("unknown")}</source>,
			<width>{input.width}</width>,
			<height>{input.height}</height>,
			<x>{input.x}</x>,
			<y>{input.y}</y>
		))
	})
	override def toMeTLText(input:NodeSeq):MeTLText = Stopwatch.time("GenericXmlSerializer.toMeTLText",() => {
		val m = utils.parseMeTLContent(input)
		val c = utils.parseCanvasContent(input)
		val tag = utils.getStringByName(input,"tag")
		val caret = utils.getIntByName(input,"caret")
		val text = utils.getStringByName(input,"text")
		val style = utils.getStringByName(input,"style")
		val family = utils.getStringByName(input,"family")
		val weight = utils.getStringByName(input,"weight")
		val size = utils.getDoubleByName(input,"size")
		val decoration = utils.getStringByName(input,"decoration")
		val color = utils.getColorByName(input,"color")
		val width = utils.getDoubleByName(input,"width")
		val height = utils.getDoubleByName(input,"height")
		val x = utils.getDoubleByName(input,"x")
		val y = utils.getDoubleByName(input,"y")
		MeTLText(config,m.author,m.timestamp,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,c.identity,c.target,c.privacy,c.slide,color)
	})
	override def fromMeTLText(input:MeTLText):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLText", () => {
		canvasContentToXml("textbox",input,List(
			<tag>{input.tag}</tag>,
			<caret>{input.caret}</caret>,
			<text>{input.text}</text>,	
			<style>{input.style}</style>,
			<family>{input.family}</family>,
			<weight>{input.weight}</weight>,
			<size>{input.size}</size>,
			<decoration>{input.decoration}</decoration>,
			<color>{ColorConverter.toHexString(input.color)}</color>,
			<width>{input.width}</width>,
			<height>{input.height}</height>,
			<x>{input.x}</x>,
			<y>{input.y}</y>
		))
	})
	override def toMeTLDirtyInk(input:NodeSeq):MeTLDirtyInk = Stopwatch.time("GenericXmlSerializer.toMeTLDirtyInk", () => {
		val m = utils.parseMeTLContent(input)
		val c = utils.parseCanvasContent(input)
		MeTLDirtyInk(config,m.author,m.timestamp,c.target,c.privacy,c.slide,c.identity)
	})
	override def fromMeTLDirtyInk(input:MeTLDirtyInk):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLDirtyInk", () => {
		canvasContentToXml("dirtyInk",input,List.empty[NodeSeq])
	})
	override def toMeTLDirtyImage(input:NodeSeq):MeTLDirtyImage = Stopwatch.time("GenericXmlSerializer.toMeTLDirtyImage", () => {
		val m = utils.parseMeTLContent(input)
		val c = utils.parseCanvasContent(input)
		MeTLDirtyImage(config,m.author,m.timestamp,c.target,c.privacy,c.slide,c.identity)
	})
	override def fromMeTLDirtyImage(input:MeTLDirtyImage):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLDirtyImage", () => {
		canvasContentToXml("dirtyImage",input,List.empty[NodeSeq])
	})
	override def toMeTLDirtyText(input:NodeSeq):MeTLDirtyText = Stopwatch.time("GenericXmlSerializer.toMeTLDirtyText", () => {
		val m = utils.parseMeTLContent(input)
		val c = utils.parseCanvasContent(input)
		MeTLDirtyText(config,m.author,m.timestamp,c.target,c.privacy,c.slide,c.identity)
	})
	override def fromMeTLDirtyText(input:MeTLDirtyText):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLDirtyText", () => {
		canvasContentToXml("dirtyText",input,List.empty[NodeSeq])
	})
	override def toMeTLCommand(input:NodeSeq):MeTLCommand = Stopwatch.time("GenericXmlSerializer.toMeTLCommand", () => {
		val m = utils.parseMeTLContent(input)
		val body = utils.getStringByName(input,"body").split(" ")
		val comm = body(0)
		val parameters = body.drop(1).toList
		MeTLCommand(config,m.author,m.timestamp,comm,parameters) 
	})
	override def fromMeTLCommand(input:MeTLCommand):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLCommand", () => {
		Text((input.command :: input.commandParameters).mkString(" "))
/*		
		metlContentToXml("command",input,List(
			<command>{input.command}</command>,
			<parameters>{input.commandParameters.map(cp => <parameter>{cp}</parameter>)}</parameters>
		))
*/
	})
	override def toSubmission(input:NodeSeq):MeTLSubmission = Stopwatch.time("GenericXmlSerializer.toSubmission", () => {
		val m = utils.parseMeTLContent(input)
		val slide = utils.getIntByName(input,"slide")
		val url = utils.getStringByName(input,"url")
		MeTLSubmission(config,m.author,m.timestamp,slide,url)
	})
	override def fromSubmission(input:MeTLSubmission):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromSubmission", () => {
		metlContentToXml("submission",input,List(
			<slide>{input.slide}</slide>,
			<url>{input.url}</url>
		))
	})
	override def toMeTLQuiz(input:NodeSeq):MeTLQuiz = Stopwatch.time("GenericXmlSerializer.toMeTLQuiz", () => {
		val m = utils.parseMeTLContent(input)
		val created = utils.getLongByName(input,"created")
		val question = utils.getStringByName(input,"question") match {
			case q if (q.length > 0) => q
			case _ => utils.getStringByName(input,"title")
		}
		val id = utils.getStringByName(input,"id")
		val url = utils.getStringByName(input,"url") match {
			case s:String if (s.length > 0 && s != "unknown url" && s != "none") => Full(s) 
			case _ => Empty
		}
		val quizImage = url.map(u => config.getResource(u))
		val isDeleted = utils.getBooleanByName(input,"isDeleted")
		val options = utils.getXmlByName(input,"quizOption").map(qo => toQuizOption(qo)).toList
		MeTLQuiz(config,m.author,m.timestamp,created,question,id,url,quizImage,isDeleted,options)	
	})
	override def fromMeTLQuiz(input:MeTLQuiz):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLQuiz", () => {
		metlContentToXml("quiz",input,List(
			<created>{input.created}</created>,
			<question>{input.question}</question>,
			<id>{input.id}</id>,
			input.url.map(u => <url>{u}</url>).openOr(NodeSeq.Empty),
			<isDeleted>{input.isDeleted}</isDeleted>,
			<options>{input.options.map(o => fromQuizOption(o))}</options>
		))
	})
	def toQuizOption(input:NodeSeq):QuizOption = Stopwatch.time("GenericXmlSerializer.toMeTLQuizOption", () => {
		val name = utils.getStringByName(input,"name")
		val text = utils.getStringByName(input,"text")
		val correct = utils.getBooleanByName(input,"correct")
		val color = utils.getColorByName(input,"color")
		QuizOption(name,text,correct,color)	
	})
	def fromQuizOption(input:QuizOption):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLQuizOption", () => {
		metlXmlToXml("quizOption",List(
			<name>{input.name}</name>,
			<text>{input.text}</text>,
			<correct>{input.correct}</correct>,
			<color>{input.color}</color>
		))	
	})
	override def toMeTLQuizResponse(input:NodeSeq):MeTLQuizResponse = Stopwatch.time("GenericXmlSerializer.toMeTLQuizResponse", () => {
		val m = utils.parseMeTLContent(input)
		val answer = utils.getStringByName(input,"answer")
		val answerer = utils.getStringByName(input,"answerer")
		val id = utils.getStringByName(input,"id")
		MeTLQuizResponse(config,m.author,m.timestamp,answer,answerer,id)
	})
	override def fromMeTLQuizResponse(input:MeTLQuizResponse):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromMeTLQuizResponse", () => {
		metlContentToXml("quizResponse",input,List(
			<answer>{input.answer}</answer>,
			<answerer>{input.answerer}</answerer>,
			<id>{input.id}</id>
		))
	})
	override def toConversation(input:NodeSeq):Conversation = Stopwatch.time("GenericXmlSerializer.toConversation",() => {
		val author = utils.getStringByName(input,"author")
		val lastAccessed = utils.getLongByName(input,"lastAccessed")
		val slides = utils.getXmlByName(input,"slide").map(s => toSlide(s)).toList
		val subject = utils.getStringByName(input,"subject")
		val tag = utils.getStringByName(input,"tag")
		val jid = utils.getIntByName(input,"jid")
		val title = utils.getStringByName(input,"title")
		val created = utils.getStringByName(input,"created")
		val permissions = utils.getXmlByName(input,"permissions").map(p => toPermissions(p)).headOption.getOrElse(Permissions.default(config))
		Conversation(config,author,lastAccessed,slides,subject,tag,jid,title,created,permissions)
	})
	override def fromConversation(input:Conversation):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromConversation", () => {
		metlXmlToXml("conversation",List(
			<author>{input.author}</author>,
			<lastAccessed>{input.lastAccessed}</lastAccessed>,
			<slides>{input.slides.map(s => fromSlide(s))}</slides>,
			<subject>{input.subject}</subject>,
			<tag>{input.tag}</tag>,
			<jid>{input.jid}</jid>,
			<title>{input.title}</title>,
			<created>{input.created}</created>,
			fromPermissions(input.permissions)
		))
	})
	override def fromConversationList(input:List[Conversation]):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromConversationList", () => {
		<conversations>{input.map(c => fromConversation(c))}</conversations>
	})
	override def toSlide(input:NodeSeq):Slide = Stopwatch.time("GenericXmlSerializer.toSlide",() => {
		val author = utils.getStringByName(input,"author")
		val id = utils.getIntByName(input,"id")
		val index = utils.getIntByName(input,"index")
		Slide(config,author,id,index)
	})
	override def fromSlide(input:Slide):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromSlide",() => {
		metlXmlToXml("slide",List(
			<id>{input.id}</id>,
			<author>{input.author}</author>,
			<index>{input.index}</index>
		))
	})
	override def toPermissions(input:NodeSeq):Permissions = Stopwatch.time("GenericXmlSerializer.toPermissions", () => {
		val studentsCanOpenFriends = utils.getBooleanByName(input,"studentCanOpenFriends")
		val studentsCanPublish = utils.getBooleanByName(input,"studentCanPublish")
		val usersAreCompulsorilySynced = utils.getBooleanByName(input,"usersAreCompulsorilySynced")
		Permissions(config,studentsCanOpenFriends,studentsCanPublish,usersAreCompulsorilySynced)	
	})
	override def fromPermissions(input:Permissions):NodeSeq = Stopwatch.time("GenericXmlSerializer.fromPermissions",() => { 
		metlXmlToXml("permissions",List(
			<studentCanOpenFriends>{input.studentsCanOpenFriends}</studentCanOpenFriends>,
			<studentCanPublish>{input.studentsCanPublish}</studentCanPublish>,
			<usersAreCompulsorilySynced>{input.usersAreCompulsorilySynced}</usersAreCompulsorilySynced>
		))
	})
	override def toColor(input:AnyRef):Color = Stopwatch.time("GenericXmlSerializer.toColor", () =>{
		Color.empty
	})
	override def fromColor(input:Color):AnyRef = "%s %s %s %s".format(input.alpha,input.red,input.green,input.blue)
	override def toPointList(input:AnyRef):List[Point] = List.empty[Point]
	override def fromPointList(input:List[Point]):AnyRef = ""
	override def toPoint(input:AnyRef):Point = {
		Point.empty
	}
	override def fromPoint(input:Point):AnyRef = "%s %s %s".format(input.x,input.y,input.thickness)
}

