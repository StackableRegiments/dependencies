package com.metl.model

abstract class Serializer {
  type T <: Object
  def fromRenderableHistory(input:History):List[T] = input.getRenderable.map(i => fromMeTLStanza(i)).toList
  def fromHistory(input:History):T = null.asInstanceOf[T]
  def toHistory(input:T):History = History.empty
  def toMeTLStanza(input:T):MeTLStanza = MeTLStanza.empty
  def toMeTLMove(input:T):MeTLMove = MeTLMove.empty
  def fromMeTLMove(input:MeTLMove):T = null.asInstanceOf[T]
  def toMeTLInk(input:T):MeTLInk = MeTLInk.empty
  def fromMeTLInk(input:MeTLInk):T = null.asInstanceOf[T]
  def toMeTLImage(input:T):MeTLImage = MeTLImage.empty
  def fromMeTLImage(input:MeTLImage):T = null.asInstanceOf[T]
  def toMeTLText(input:T):MeTLText = MeTLText.empty
  def fromMeTLText(input:MeTLText):T = null.asInstanceOf[T]
  def toMeTLDirtyInk(input:T):MeTLDirtyInk = MeTLDirtyInk.empty
  def fromMeTLDirtyInk(input:MeTLDirtyInk):T = null.asInstanceOf[T]
  def toMeTLDirtyImage(input:T):MeTLDirtyImage = MeTLDirtyImage.empty
  def fromMeTLDirtyImage(input:MeTLDirtyImage):T = null.asInstanceOf[T]
  def toMeTLDirtyText(input:T):MeTLDirtyText = MeTLDirtyText.empty
  def fromMeTLDirtyText(input:MeTLDirtyText):T = null.asInstanceOf[T]
  def toMeTLCommand(input:T):MeTLCommand = MeTLCommand.empty
  def fromMeTLCommand(input:MeTLCommand):T = null.asInstanceOf[T]
  def toSubmission(input:T):MeTLSubmission = MeTLSubmission.empty
  def fromSubmission(input:MeTLSubmission):T = null.asInstanceOf[T]
  def toMeTLQuiz(input:T):MeTLQuiz = MeTLQuiz.empty
  def fromMeTLQuiz(input:MeTLQuiz):T = null.asInstanceOf[T]
  def toMeTLQuizResponse(input:T):MeTLQuizResponse = MeTLQuizResponse.empty
  def fromMeTLQuizResponse(input:MeTLQuizResponse):T = null.asInstanceOf[T]
  def toConversation(input:T):Conversation = Conversation.empty
  def fromConversation(input:Conversation):T = null.asInstanceOf[T]
  def fromConversationList(input:List[Conversation]):T = null.asInstanceOf[T]
  def toSlide(input:T):Slide = Slide.empty
  def fromSlide(input:Slide):T = null.asInstanceOf[T]
  def toPermissions(input:T):Permissions = Permissions.empty
  def fromPermissions(input:Permissions):T = null.asInstanceOf[T]
  def toPoint(input:AnyRef):Point = Point.empty
  def fromPoint(input:Point):AnyRef = null.asInstanceOf[T]
  def toPointList(input:AnyRef):List[Point] = List.empty[Point]
  def fromPointList(input:List[Point]):AnyRef = null.asInstanceOf[T]
  def toColor(input:AnyRef):Color = Color.empty
  def fromColor(input:Color):AnyRef = null.asInstanceOf[T]
  def fromMeTLStanza(input:MeTLStanza):T = input match {
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
    case m:MeTLMove => fromMeTLMove(m)
    case _ => null.asInstanceOf[T]
  }
}

class PassthroughSerializer extends Serializer {
  type T = Object
  override def toMeTLStanza(input:Object):MeTLStanza = input.asInstanceOf[MeTLStanza]
  override def fromMeTLStanza(input:MeTLStanza):Object = input.asInstanceOf[Object]
  override def toMeTLInk(input:Object):MeTLInk = input.asInstanceOf[MeTLInk]
  override def fromMeTLInk(input:MeTLInk):Object = input.asInstanceOf[Object]
  override def toMeTLImage(input:Object):MeTLImage = input.asInstanceOf[MeTLImage]
  override def fromMeTLImage(input:MeTLImage):Object = input.asInstanceOf[Object]
  override def toMeTLText(input:Object):MeTLText = input.asInstanceOf[MeTLText]
  override def fromMeTLText(input:MeTLText):Object = input.asInstanceOf[Object]
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
  override def toPermissions(input:Object):Permissions = input.asInstanceOf[Permissions]
  override def fromPermissions(input:Permissions):Object = input.asInstanceOf[Object]
}
