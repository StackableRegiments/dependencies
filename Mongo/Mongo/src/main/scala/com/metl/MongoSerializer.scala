package com.metl.model

import com.mongodb._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import Privacy._

class MongoSerializerHelper {
	def obj(name:String,obj:DBObject) = obj.get(name).asInstanceOf[DBObject]
	def string(name:String,obj:DBObject) = obj.get(name).asInstanceOf[String]
	def long(name:String,obj:DBObject) = obj.get(name).asInstanceOf[Long]
	def int(name:String,obj:DBObject) = obj.get(name).asInstanceOf[Int]
	def double(name:String,obj:DBObject) = obj.get(name).asInstanceOf[Double]
	def bool(name:String,obj:DBObject) = obj.get(name).asInstanceOf[Boolean]
	def points(name:String,obj:DBObject) = obj.get(name).asInstanceOf[BasicDBList]
	def toDBType(input:Any):AnyRef = input match {
		case l:BasicDBList => l
		case d:DBObject => d
		case s:String => s
		case o:Object => o
		case obj => obj.asInstanceOf[AnyRef]
	}
}

class MongoSerializer(configName:String) extends Serializer{
	type T = DBObject
	lazy val config = ServerConfiguration.configForName(configName)
	val u = new MongoSerializerHelper

	private def parseMeTLContent(input:MeTLStanza):List[(String,Any)] = {
		List(
			("author",input.author),
			("timestamp",input.timestamp)
		)
	}
	private def parseCanvasContent(input:MeTLCanvasContent):List[(String,Any)] = {
		List(
			("target",input.target),
			("privacy",input.privacy.toString.toLowerCase),
			("slide",input.slide),
			("identity",input.identity)
		)
	}
  private def parseObjForMeTLContent(input:DBObject):ParsedMeTLContent = {
		val author = u.string("author",input)
		val timestamp = u.long("timestamp",input)
    ParsedMeTLContent(author,timestamp)
  }
  private def parseObjForCanvasContent(input:DBObject):ParsedCanvasContent = {
		val target = u.string("target",input)
		val privacy = Privacy.parse(u.string("privacy",input))
		val slide = u.string("slide",input)
		val identity = u.string("identity",input)
    ParsedCanvasContent(target,privacy,slide,identity)
  }
  private def hasField(input:DBObject,fieldName:String) = Stopwatch.time("MongoSerializer.has", () => {
		input.containsField(fieldName)
  })
  private def isOfType(input:DBObject,name:String) = Stopwatch.time("MongoSerializer.isOfType", () => {
    input.get("type") == name
  })
	private def constructObj(name:String,fields:List[(String,Any)]):DBObject = {
		val obj = new BasicDBObject()
		(("type",name) :: fields).foreach(i => obj.put(i._1,u.toDBType(i._2)))
		obj.asInstanceOf[DBObject]
	}
	override def toMeTLStanza(input:DBObject):MeTLStanza = MeTLStanza.empty
	override def toMeTLInk(input:DBObject):MeTLInk = Stopwatch.time("MongoSerializer.toMeTLInk", () => {
		val mc = parseObjForMeTLContent(input)
		val cc = parseObjForCanvasContent(input)
		val checksum = u.double("checksum",input)
		val startingSum = u.double("startingSum",input)
		val points = toPointList(u.points("points",input))
		val color = toColor(u.obj("color",input))
		val thickness = u.double("thickness",input)
		val isHighlighter = u.bool("isHighlighter",input)
    MeTLInk(config,mc.author,mc.timestamp,checksum,startingSum,points,color,thickness,isHighlighter,cc.target,cc.privacy,cc.slide,startingSum.toString)
	})
	override def fromMeTLInk(input:MeTLInk):DBObject = Stopwatch.time("MongoSerializer.fromMeTLInk", () => {
		constructObj(
			"ink", parseMeTLContent(input) ::: List(
				("checksum",input.checksum),
				("startingSum",input.startingSum),
				("points",fromPointList(input.points)),
				("color",fromColor(input.color)),
				("thickness",input.thickness),
				("isHighlighter",input.isHighlighter)
			) ::: parseCanvasContent(input)
		)
	})

	override def toMeTLImage(input:DBObject):MeTLImage = Stopwatch.time("MongoSerializer.toMeTLImage", () => {
		val mc = parseObjForMeTLContent(input)
		val cc = parseObjForCanvasContent(input)
		val tag = u.string("tag",input)
		val source = Full(u.string("source",input))
		val imageBytes = source.map(u => config.getResource(u))
		val pngBytes = Empty
		val width = u.double("width",input)
		val height = u.double("height",input)
		val x = u.double("x",input)
		val y = u.double("y",input)
		MeTLImage(config,mc.author,mc.timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,cc.target,cc.privacy,cc.slide,cc.identity)
	})
	override def fromMeTLImage(input:MeTLImage):DBObject = Stopwatch.time("MongoSerializer.fromMeTLImage", () => {
		constructObj(
			"image", parseMeTLContent(input) ::: List(
				("tag",input.tag),
				("width",input.width),
				("height",input.height),
				("x",input.x),
				("y",input.y)
			) ::: input.source.map(url => List(("url",url))).openOr(List.empty[(String,Object)]) ::: parseCanvasContent(input)
		)	
	})
	override def toMeTLText(input:DBObject):MeTLText = Stopwatch.time("MongoSerializer.toMeTLText", () => {
		val mc = parseObjForMeTLContent(input)
		val cc = parseObjForCanvasContent(input)
		val text = u.string("text",input)
		val height = u.double("height",input)
		val width = u.double("width",input)
		val caret = u.int("caret",input)
		val x = u.double("x",input)
		val y = u.double("y",input)
		val tag = u.string("tag",input)
		val style = u.string("style",input)
		val family = u.string("family",input)
		val weight = u.string("weight",input)
		val size = u.double("size",input)
		val decoration = u.string("decoration",input)
		val color = toColor(u.obj("color",input))
	  MeTLText(config,mc.author,mc.timestamp,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,cc.identity,cc.target,cc.privacy,cc.slide,color)
	})
	override def fromMeTLText(input:MeTLText):DBObject = Stopwatch.time("MongoSerializer.fromMeTLText", () => {
		constructObj(
			"text", parseMeTLContent(input) ::: List(
				("text",input.text),
				("width",input.width),
				("height",input.height),
				("x",input.x),
				("y",input.y),
				("caret",input.caret),
				("tag",input.tag),
				("style",input.style),
				("family",input.family),
				("weight",input.weight),
				("size",input.size),
				("decoration",input.decoration),
				("color",fromColor(input.color))
			) ::: parseCanvasContent(input)
		)
	})
	override def toMeTLDirtyInk(input:DBObject):MeTLDirtyInk = Stopwatch.time("MongoSerializer.toMeTLDirtyInk", () => {
		val mc = parseObjForMeTLContent(input)
		val cc = parseObjForCanvasContent(input)
		MeTLDirtyInk(config,mc.author,mc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity)
	})
	override def fromMeTLDirtyInk(input:MeTLDirtyInk):DBObject = Stopwatch.time("MongoSerializer.fromDirtyImage", () => {
		constructObj("dirtyInk",parseMeTLContent(input) ::: parseCanvasContent(input))
	})
	override def toMeTLDirtyImage(input:DBObject):MeTLDirtyImage = Stopwatch.time("MongoSerializer.toMeTLDirtyImage", () => {
		val mc = parseObjForMeTLContent(input)
		val cc = parseObjForCanvasContent(input)
		MeTLDirtyImage(config,mc.author,mc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity)
	})
	override def fromMeTLDirtyImage(input:MeTLDirtyImage):DBObject = Stopwatch.time("MongoSerializer.fromMeTLDirtyImage", () => {
		constructObj("dirtyImage",parseMeTLContent(input) ::: parseCanvasContent(input))
	})
	override def toMeTLDirtyText(input:DBObject):MeTLDirtyText = Stopwatch.time("MongoSerializer.toMeTLDirtyText", () => {
		val mc = parseObjForMeTLContent(input)
		val cc = parseObjForCanvasContent(input)
		MeTLDirtyText(config,mc.author,mc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity)
	})
	override def fromMeTLDirtyText(input:MeTLDirtyText):DBObject = Stopwatch.time("MongoSerializer.fromMeTLDirtyText", () => {
		constructObj("dirtyText",parseMeTLContent(input) ::: parseCanvasContent(input))
	})
	override def toMeTLCommand(input:DBObject):MeTLCommand = MeTLCommand.empty
	override def fromMeTLCommand(input:MeTLCommand):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toSubmission(input:DBObject):MeTLSubmission = MeTLSubmission.empty
	override def fromSubmission(input:MeTLSubmission):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toMeTLQuiz(input:DBObject):MeTLQuiz = MeTLQuiz.empty
	override def fromMeTLQuiz(input:MeTLQuiz):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toMeTLQuizResponse(input:DBObject):MeTLQuizResponse = MeTLQuizResponse.empty
	override def fromMeTLQuizResponse(input:MeTLQuizResponse):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toConversation(input:DBObject):Conversation = Conversation.empty
	override def fromConversation(input:Conversation):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toSlide(input:DBObject):Slide = Slide.empty
	override def fromSlide(input:Slide):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toPermissions(input:DBObject):Permissions = Permissions.default(config)
	override def fromPermissions(input:Permissions):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toColor(input:AnyRef):Color = Color.empty
	override def fromColor(input:Color):AnyRef = new BasicDBObject().asInstanceOf[DBObject]
	override def toPoint(input:AnyRef):Point = Point.empty
	override def fromPoint(input:Point):AnyRef = new BasicDBObject().asInstanceOf[DBObject]
	override def toPointList(input:AnyRef):List[Point] = List.empty[Point]
	override def fromPointList(input:List[Point]):AnyRef = new BasicDBObject().asInstanceOf[DBObject]
}

