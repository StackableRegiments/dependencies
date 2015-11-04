package com.metl.data

import com.metl.utils._

class SerializationNotImplementedException extends Exception("Serializer has not implemented this method")

abstract class Serializer {
  type T <: Object
  def fromRenderableHistory(input:History):List[T] = input.getRenderable.map(i => fromMeTLData(i)).toList
  def fromHistory(input:History):T = throw new SerializationNotImplementedException
  def toHistory(input:T):History = throw new SerializationNotImplementedException
  def toMeTLData(input:T):MeTLData = throw new SerializationNotImplementedException
  def toMeTLMoveDelta(input:T):MeTLMoveDelta = throw new SerializationNotImplementedException
  def fromMeTLMoveDelta(input:MeTLMoveDelta):T = throw new SerializationNotImplementedException
  def toMeTLAttendance(input:T):Attendance = throw new SerializationNotImplementedException
  def fromMeTLAttendance(input:Attendance):T = throw new SerializationNotImplementedException
  def toMeTLInk(input:T):MeTLInk = throw new SerializationNotImplementedException
  def fromMeTLInk(input:MeTLInk):T = throw new SerializationNotImplementedException
  def toMeTLImage(input:T):MeTLImage = throw new SerializationNotImplementedException
  def fromMeTLImage(input:MeTLImage):T = throw new SerializationNotImplementedException
  def toMeTLText(input:T):MeTLText = throw new SerializationNotImplementedException
  def fromMeTLText(input:MeTLText):T = throw new SerializationNotImplementedException
  def toMeTLDirtyInk(input:T):MeTLDirtyInk = throw new SerializationNotImplementedException
  def fromMeTLDirtyInk(input:MeTLDirtyInk):T = throw new SerializationNotImplementedException
  def toMeTLDirtyImage(input:T):MeTLDirtyImage = throw new SerializationNotImplementedException
  def fromMeTLDirtyImage(input:MeTLDirtyImage):T = throw new SerializationNotImplementedException
  def toMeTLDirtyText(input:T):MeTLDirtyText = throw new SerializationNotImplementedException
  def fromMeTLDirtyText(input:MeTLDirtyText):T = throw new SerializationNotImplementedException
  def toMeTLCommand(input:T):MeTLCommand = throw new SerializationNotImplementedException
  def fromMeTLCommand(input:MeTLCommand):T = throw new SerializationNotImplementedException
  def toSubmission(input:T):MeTLSubmission = throw new SerializationNotImplementedException
  def fromSubmission(input:MeTLSubmission):T = throw new SerializationNotImplementedException
  def toMeTLQuiz(input:T):MeTLQuiz = throw new SerializationNotImplementedException
  def fromMeTLQuiz(input:MeTLQuiz):T = throw new SerializationNotImplementedException
  def toMeTLQuizResponse(input:T):MeTLQuizResponse = throw new SerializationNotImplementedException
  def fromMeTLQuizResponse(input:MeTLQuizResponse):T = throw new SerializationNotImplementedException
  def toConversation(input:T):Conversation = throw new SerializationNotImplementedException
  def fromConversation(input:Conversation):T = throw new SerializationNotImplementedException
  def fromConversationList(input:List[Conversation]):T = throw new SerializationNotImplementedException
  def toSlide(input:T):Slide = throw new SerializationNotImplementedException
  def fromSlide(input:Slide):T = throw new SerializationNotImplementedException
  def toGroupSet(input:T):GroupSet = throw new SerializationNotImplementedException
  def fromGroupSet(input:GroupSet):T = throw new SerializationNotImplementedException
  def toGroup(input:T):Group = throw new SerializationNotImplementedException
  def fromGroup(input:Group):T = throw new SerializationNotImplementedException
  def toGroupingStrategy(input:T):GroupingStrategy = throw new SerializationNotImplementedException
  def fromGroupingStrategy(input:GroupingStrategy):T = throw new SerializationNotImplementedException
  def toPermissions(input:T):Permissions = throw new SerializationNotImplementedException
  def fromPermissions(input:Permissions):T = throw new SerializationNotImplementedException
  def toPoint(input:AnyRef):Point = throw new SerializationNotImplementedException
  def fromPoint(input:Point):AnyRef = throw new SerializationNotImplementedException
  def toPointList(input:AnyRef):List[Point] = throw new SerializationNotImplementedException
  def fromPointList(input:List[Point]):AnyRef = throw new SerializationNotImplementedException
  def toColor(input:AnyRef):Color = throw new SerializationNotImplementedException
  def fromColor(input:Color):AnyRef = throw new SerializationNotImplementedException
  def toMeTLUnhandledStanza(input:T):MeTLUnhandledStanza[T] = throw new SerializationNotImplementedException
  def fromMeTLUnhandledStanza(input:MeTLUnhandledStanza[T]):T = throw new SerializationNotImplementedException
  def toMeTLUnhandledData(input:T):MeTLUnhandledData[T] = throw new SerializationNotImplementedException
  def fromMeTLUnhandledData(input:MeTLUnhandledData[T]):T = throw new SerializationNotImplementedException
  def toMeTLUnhandledCanvasContent(input:T):MeTLUnhandledCanvasContent[T] = throw new SerializationNotImplementedException
  def fromMeTLUnhandledCanvasContent(input:MeTLUnhandledCanvasContent[T]):T = throw new SerializationNotImplementedException
  def fromMeTLData(input:MeTLData):T = input match {
    case i:MeTLInk => fromMeTLInk(i)
    case t:MeTLText => fromMeTLText(t)
    case i:MeTLImage => fromMeTLImage(i)
    case d:MeTLDirtyInk => fromMeTLDirtyInk(d)
    case d:MeTLDirtyText => fromMeTLDirtyText(d)
    case d:MeTLDirtyImage => fromMeTLDirtyImage(d)
    case c:MeTLCommand => fromMeTLCommand(c)
    case q:MeTLQuiz => fromMeTLQuiz(q)
    case qr:MeTLQuizResponse => fromMeTLQuizResponse(qr)
    case s:MeTLSubmission => fromSubmission(s)
    case m:MeTLMoveDelta => fromMeTLMoveDelta(m)
    case a:Attendance => fromMeTLAttendance(a)
    case cc:MeTLUnhandledCanvasContent[T] => fromMeTLUnhandledCanvasContent(cc)
    case ms:MeTLUnhandledStanza[T] => fromMeTLUnhandledStanza(ms)
    case mx:MeTLUnhandledData[T] => fromMeTLUnhandledData(mx)
    case _ => throw new SerializationNotImplementedException
  }
}

class PassthroughSerializer extends Serializer {
  type T = Object
  override def toMeTLData(input:Object):MeTLData = input.asInstanceOf[MeTLData]
  override def fromMeTLData(input:MeTLData):Object = input.asInstanceOf[Object]
  override def toMeTLAttendance(input:T):Attendance = input.asInstanceOf[Attendance]
  override def fromMeTLAttendance(input:Attendance):T = input.asInstanceOf[Object]
  override def toMeTLInk(input:Object):MeTLInk = input.asInstanceOf[MeTLInk]
  override def fromMeTLInk(input:MeTLInk):Object = input.asInstanceOf[Object]
  override def toMeTLImage(input:Object):MeTLImage = input.asInstanceOf[MeTLImage]
  override def fromMeTLImage(input:MeTLImage):Object = input.asInstanceOf[Object]
  override def toMeTLText(input:Object):MeTLText = input.asInstanceOf[MeTLText]
  override def fromMeTLText(input:MeTLText):Object = input.asInstanceOf[Object]
	override def toMeTLMoveDelta(input:Object):MeTLMoveDelta = input.asInstanceOf[MeTLMoveDelta]
	override def fromMeTLMoveDelta(input:MeTLMoveDelta):Object = input.asInstanceOf[Object]
  override def toMeTLDirtyInk(input:Object):MeTLDirtyInk = input.asInstanceOf[MeTLDirtyInk]
  override def fromMeTLDirtyInk(input:MeTLDirtyInk):Object = input.asInstanceOf[Object]
  override def toMeTLDirtyImage(input:Object):MeTLDirtyImage = input.asInstanceOf[MeTLDirtyImage]
  override def fromMeTLDirtyImage(input:MeTLDirtyImage):Object = input.asInstanceOf[Object]
  override def toMeTLDirtyText(input:Object):MeTLDirtyText = input.asInstanceOf[MeTLDirtyText]
  override def fromMeTLDirtyText(input:MeTLDirtyText):Object = input.asInstanceOf[Object]
  override def toMeTLCommand(input:Object):MeTLCommand = input.asInstanceOf[MeTLCommand]
  override def fromMeTLCommand(input:MeTLCommand):Object = input.asInstanceOf[Object]
  override def toSubmission(input:Object):MeTLSubmission = input.asInstanceOf[MeTLSubmission]
  override def fromSubmission(input:MeTLSubmission):Object = input.asInstanceOf[Object]
  override def toMeTLQuiz(input:Object):MeTLQuiz = input.asInstanceOf[MeTLQuiz]
  override def fromMeTLQuiz(input:MeTLQuiz):Object = input.asInstanceOf[Object]
  override def toMeTLQuizResponse(input:Object):MeTLQuizResponse = input.asInstanceOf[MeTLQuizResponse]
  override def fromMeTLQuizResponse(input:MeTLQuizResponse):Object = input.asInstanceOf[Object]
  override def toConversation(input:Object):Conversation = input.asInstanceOf[Conversation]
  override def fromConversation(input:Conversation):Object = input.asInstanceOf[Object]
  override def toSlide(input:Object):Slide = input.asInstanceOf[Slide]
  override def fromSlide(input:Slide):Object = input.asInstanceOf[Object]
  override def toGroupSet(input:T):GroupSet = input.asInstanceOf[GroupSet]
  override def fromGroupSet(input:GroupSet):T = input.asInstanceOf[Object]
  override def toGroup(input:T):Group = input.asInstanceOf[Group]
  override def fromGroup(input:Group):T = input.asInstanceOf[Object]
  override def toGroupingStrategy(input:T):GroupingStrategy = input.asInstanceOf[GroupingStrategy]
  override def fromGroupingStrategy(input:GroupingStrategy):T = input.asInstanceOf[Object]
  override def toPermissions(input:Object):Permissions = input.asInstanceOf[Permissions]
  override def fromPermissions(input:Permissions):Object = input.asInstanceOf[Object]
	override def toPoint(input:AnyRef):Point = input.asInstanceOf[Point]
  override def fromPoint(input:Point):AnyRef = input.asInstanceOf[AnyRef]
  override def toPointList(input:AnyRef):List[Point] = input.asInstanceOf[List[Point]]
  override def fromPointList(input:List[Point]):AnyRef = input.asInstanceOf[AnyRef]
  override def toColor(input:AnyRef):Color = input.asInstanceOf[Color]
  override def fromColor(input:Color):AnyRef = input.asInstanceOf[AnyRef]
  override def toMeTLUnhandledData(input:AnyRef):MeTLUnhandledData[T] = input.asInstanceOf[MeTLUnhandledData[T]]
  override def fromMeTLUnhandledData(input:MeTLUnhandledData[T]):T = input.asInstanceOf[Object]
  override def toMeTLUnhandledCanvasContent(input:AnyRef):MeTLUnhandledCanvasContent[T] = input.asInstanceOf[MeTLUnhandledCanvasContent[T]]
  override def fromMeTLUnhandledCanvasContent(input:MeTLUnhandledCanvasContent[T]):T = input.asInstanceOf[Object]
}
