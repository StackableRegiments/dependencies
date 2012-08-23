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
	def listOfObj(name:String,obj:DBObject) = obj.get(name).asInstanceOf[List[DBObject]]
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
		putFields(new BasicDBObject(),("type",name) :: fields)
	}
	private def putFields(obj:DBObject,fields:List[(String,Any)]):DBObject = {
		fields.foreach(i => obj.put(i._1,u.toDBType(i._2)))
		obj.asInstanceOf[DBObject]
	}
	override def toMeTLStanza(input:DBObject):MeTLStanza = MeTLStanza.empty
	override def toMeTLInk(input:DBObject):MeTLInk = Stopwatch.time("MongoSerializer.toMeTLInk", () => {
		val mc = parseObjForMeTLContent(input)
		val cc = parseObjForCanvasContent(input)
		val checksum = u.double("checksum",input)
		val startingSum = u.double("startingSum",input)
		val points = toPointList(u.listOfObj("points",input))
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
	override def toMeTLCommand(input:DBObject):MeTLCommand = Stopwatch.time("MongoSerializer.toMeTLCommand", () => {
		MeTLCommand.empty
	})
	override def fromMeTLCommand(input:MeTLCommand):DBObject = Stopwatch.time("MongoSerializer.fromMeTLCommand", () => {
		constructObj("command",parseMeTLContent(input))
	})
	override def toSubmission(input:DBObject):MeTLSubmission = Stopwatch.time("MongoSerializer.toSubmission", () => {
		MeTLSubmission.empty
	})
	override def fromSubmission(input:MeTLSubmission):DBObject = Stopwatch.time("MongoSerializer.", () => {
		constructObj("submission",parseMeTLContent(input))
	})
	override def toMeTLQuiz(input:DBObject):MeTLQuiz = Stopwatch.time("MongoSerializer.toMeTLQuiz", () => {
		MeTLQuiz.empty
	})
	override def fromMeTLQuiz(input:MeTLQuiz):DBObject = Stopwatch.time("MongoSerializer.", () => {
		constructObj("quiz",parseMeTLContent(input))
	})
	override def toMeTLQuizResponse(input:DBObject):MeTLQuizResponse = Stopwatch.time("MongoSerializer.toMeTLQuizResponse", () => {
		MeTLQuizResponse.empty
	})
	override def fromMeTLQuizResponse(input:MeTLQuizResponse):DBObject =  Stopwatch.time("MongoSerializer.", () => {
		constructObj("quizResponse", parseMeTLContent(input))
	})
	override def toConversation(input:DBObject):Conversation = Stopwatch.time("MongoSerializer.toConversation", () => {
		val author = u.string("author",input)
		val lastAccessed = u.long("lastAccessed",input)
		val slides = u.listOfObj("slides",input).map(i => toSlide(i)).toList
		val subject = u.string("subject",input)
		val tag = u.string("tag",input)
		val jid = u.int("jid",input)
		val title = u.string("title",input)
		val created = u.string("created",input)
		val permissions = toPermissions(u.obj("permissions",input))
    Conversation(config,author,lastAccessed,slides,subject,tag,jid,title,created,permissions)
	})
	override def fromConversation(input:Conversation):DBObject = Stopwatch.time("MongoSerializer.", () => {
		putFields(new BasicDBObject(), List(
			("author",input.author),
			("lastAccessed",input.lastAccessed),
			("slides",input.slides.map(i => fromSlide(i)).toList),
			("subject",input.subject),
			("tag",input.tag),
			("jid",input.jid),
			("title",input.title),
			("created",input.created),
			("permissions",fromPermissions(input.permissions))	
		))
	})
	override def toSlide(input:DBObject):Slide = Stopwatch.time("MongoSerializer.toSlide", () => {
		val author = u.string("author",input)
		val id = u.int("id",input)
		val index = u.int("index",input)
		Slide(config,author,id,index)
	})
	override def fromSlide(input:Slide):DBObject = Stopwatch.time("MongoSerializer.", () => {
		putFields(new BasicDBObject(), List(("id",input.id),("author",input.author),("index",input.index)))
	})
	override def toPermissions(input:DBObject):Permissions = Stopwatch.time("MongoSerializer.toPermissions", () => {
		val scp = u.bool("studentsCanPublish",input)
		val scof = u.bool("studentsCanOpenFriends",input)
		val uacs = u.bool("usersAreCompusorilySynced",input)
		Permissions(config,scof,scp,uacs)
	})
	override def fromPermissions(input:Permissions):DBObject = Stopwatch.time("MongoSerializer.", () => {
		putFields(new BasicDBObject(), List(("studentsCanPublish",input.studentsCanPublish),("studentsCanOpenFriends",input.studentsCanOpenFriends),("usersAreCompusorilySynced",input.usersAreCompulsorilySynced)))
	})
	override def toColor(input:AnyRef):Color = Stopwatch.time("MongoSerializer.toColor", () => {
		val obj = input.asInstanceOf[DBObject]
		val a = u.int("a",obj)
		val r = u.int("r",obj)
		val g = u.int("g",obj)
		val b = u.int("b",obj)
		Color(a,r,g,b)
	})
	override def fromColor(input:Color):AnyRef = Stopwatch.time("MongoSerializer.", () => {
		putFields(new BasicDBObject(), List(("a",input.alpha),("r",input.red),("g",input.green),("b",input.blue)))
	})
	override def toPoint(input:AnyRef):Point = Stopwatch.time("MongoSerializer.toPoint", () => {
		val obj = input.asInstanceOf[DBObject]
		val x = u.double("x",obj)
		val y = u.double("y",obj)
		val thickness = u.double("p",obj)
		Point(x,y,thickness)
	})
	override def fromPoint(input:Point):AnyRef = Stopwatch.time("MongoSerializer.", () => {
		putFields(new BasicDBObject(), List(("x",input.x),("y",input.y),("p",input.thickness)))
	})
	override def toPointList(input:AnyRef):List[Point] = Stopwatch.time("MongoSerializer.toPointList", () => {
		input.asInstanceOf[List[DBObject]].map(i => toPoint(i)).toList
	})
	override def fromPointList(input:List[Point]):AnyRef = Stopwatch.time("MongoSerializer.", () => {
		input.map(i => fromPoint(i)).toList
	})
}

