package com.metl.model

import com.mongodb._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import Privacy._

class MongoSerializer(configName:String) extends Serializer{
	type T = DBObject
	lazy val config = ServerConfiguration.configForName(configName)

	override def toMeTLStanza(input:DBObject):MeTLStanza = MeTLStanza.empty
	override def toMeTLInk(input:DBObject):MeTLInk = MeTLInk.empty
	override def fromMeTLInk(input:MeTLInk):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toMeTLImage(input:DBObject):MeTLImage = MeTLImage.empty
	override def fromMeTLImage(input:MeTLImage):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toMeTLText(input:DBObject):MeTLText = MeTLText.empty
	override def fromMeTLText(input:MeTLText):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toMeTLDirtyInk(input:DBObject):MeTLDirtyInk = MeTLDirtyInk.empty
	override def fromMeTLDirtyInk(input:MeTLDirtyInk):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toMeTLDirtyImage(input:DBObject):MeTLDirtyImage = MeTLDirtyImage.empty
	override def fromMeTLDirtyImage(input:MeTLDirtyImage):DBObject = new BasicDBObject().asInstanceOf[DBObject]
	override def toMeTLDirtyText(input:DBObject):MeTLDirtyText = MeTLDirtyText.empty
	override def fromMeTLDirtyText(input:MeTLDirtyText):DBObject = new BasicDBObject().asInstanceOf[T]
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

