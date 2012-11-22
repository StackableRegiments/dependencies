package com.metl.h2.dbformats

import net.liftweb.mapper._

import com.metl.data._

object H2Constants{
	val maxStanza = 65565
	val author = 64
	val room = 64
	val target = 64
	val identity = 256
	val color = 9
	val url = 512
	val tag = 256
	val privacy = 20
}

trait H2MeTLContent[C <:H2MeTLContent[C]] extends LongKeyedMapper[C] with IdPK{
	self: C =>
	object metlType extends MappedString[C](this,13)
	object room extends MappedString[C](this,H2Constants.room)
}

trait H2MeTLStanza[C <: H2MeTLStanza[C]] extends H2MeTLContent[C]{
	self: C =>
	object timestamp extends MappedLong[C](this)
	object author extends MappedString[C](this,H2Constants.author)
}

trait H2MeTLCanvasContent[C <: H2MeTLCanvasContent[C]] extends H2MeTLStanza[C] {
	self: C =>
	object target extends MappedString[C](this,H2Constants.target)
	object privacy extends MappedString[C](this,H2Constants.privacy)
	object slide extends MappedString[C](this,H2Constants.room)
	object identity extends MappedString[C](this,H2Constants.identity)
}

class H2Ink extends H2MeTLCanvasContent[H2Ink] {
	def getSingleton = H2Ink
	object checksum extends MappedDouble(this)
	object startingSum extends MappedDouble(this)
	object points extends MappedText(this)
	object color extends MappedString(this,H2Constants.color)
	object thickness extends MappedDouble(this)
	object isHighlighter extends MappedBoolean(this)
}
object H2Ink extends H2Ink with LongKeyedMetaMapper[H2Ink] {
}
class H2Text extends H2MeTLCanvasContent[H2Text] {
	def getSingleton = H2Text
	object text extends MappedText(this)
	object height extends MappedDouble(this)
	object width extends MappedDouble(this)
	object caret extends MappedInt(this)
	object x extends MappedDouble(this)
	object y extends MappedDouble(this)
	object tag extends MappedString(this,H2Constants.tag)
	object style extends MappedString(this,64)
	object family extends MappedString(this,128)
	object weight extends MappedString(this,64)
	object size extends MappedDouble(this)
	object decoration extends MappedString(this,64)
	object color extends MappedString(this,H2Constants.color)
}
object H2Text extends H2Text with LongKeyedMetaMapper[H2Text] {
}
class H2Image extends H2MeTLCanvasContent[H2Image] {
	def getSingleton = H2Image
	object tag extends MappedString(this,H2Constants.tag)
	object source extends MappedString(this,H2Constants.url)
	object width extends MappedDouble(this)
	object height extends MappedDouble(this)
	object x extends MappedDouble(this)
	object y extends MappedDouble(this)
}
object H2Image extends H2Image with LongKeyedMetaMapper[H2Image] {
}
class H2DirtyInk extends H2MeTLCanvasContent[H2DirtyInk] {
	def getSingleton = H2DirtyInk
}
object H2DirtyInk extends H2DirtyInk with LongKeyedMetaMapper[H2DirtyInk]{
}
class H2DirtyText extends H2MeTLCanvasContent[H2DirtyText] {
	def getSingleton = H2DirtyText
}
object H2DirtyText extends H2DirtyText with LongKeyedMetaMapper[H2DirtyText]{
}
class H2DirtyImage extends H2MeTLCanvasContent[H2DirtyImage] {
	def getSingleton = H2DirtyImage
}
object H2DirtyImage extends H2DirtyImage with LongKeyedMetaMapper[H2DirtyImage]{
}
class H2MoveDelta extends H2MeTLCanvasContent[H2MoveDelta]{
	def getSingleton = H2MoveDelta
	object inkIds extends MappedText(this)
	object textIds extends MappedText(this)
	object imageIds extends MappedText(this)
	object xTranslate extends MappedDouble(this)
	object yTranslate extends MappedDouble(this)
	object xScale extends MappedDouble(this)
	object yScale extends MappedDouble(this)
	object newPrivacy extends MappedString(this,H2Constants.privacy)
	object isDeleted extends MappedBoolean(this)
}
object H2MoveDelta extends H2MoveDelta with LongKeyedMetaMapper[H2MoveDelta]{
}
class H2Quiz extends H2MeTLStanza[H2Quiz]{
	def getSingleton = H2Quiz
	object created extends MappedLong(this)
	object question extends MappedText(this)
	object quizId extends MappedString(this,H2Constants.identity)
	object url extends MappedString(this,H2Constants.url)
	object isDeleted extends MappedBoolean(this)
	object options extends MappedText(this)
}
object H2Quiz extends H2Quiz with LongKeyedMetaMapper[H2Quiz]{
}
class H2QuizResponse extends H2MeTLStanza[H2QuizResponse]{
	def getSingleton = H2QuizResponse
	object answer extends MappedString(this,8)
	object answerer extends MappedString(this,H2Constants.author)
	object quizId extends MappedString(this,H2Constants.identity)
}
object H2QuizResponse extends H2QuizResponse with LongKeyedMetaMapper[H2QuizResponse]{
}
class H2Command extends H2MeTLStanza[H2Command]{
	def getSingleton = H2Command
	object command extends MappedString(this,128)
	object commandParameters extends MappedText(this)
}
object H2Command extends H2Command with LongKeyedMetaMapper[H2Command]{
}
class H2Submission extends H2MeTLCanvasContent[H2Submission]{
	def getSingleton = H2Submission
	object title extends MappedString(this,512)
	object slideJid extends MappedInt(this)
	object url extends MappedString(this,H2Constants.url)
}
object H2Submission extends H2Submission with LongKeyedMetaMapper[H2Submission]{
}
class H2Conversation extends H2MeTLContent[H2Conversation]{
	def getSingleton = H2Conversation
	object author extends MappedString(this,H2Constants.author)
	object lastAccessed extends MappedLong(this)
	object subject extends MappedString(this,64)
	object tag extends MappedString(this,H2Constants.tag)
	object jid extends MappedInt(this)
	object title extends MappedString(this,512)
	object created extends MappedString(this,64)
	object permissions extends MappedString(this,64)
	object blackList extends MappedText(this)
	object slides extends MappedText(this)
}
object H2Conversation extends H2Conversation with LongKeyedMetaMapper[H2Conversation]{
}
class H2Resource extends H2MeTLContent[H2Resource]{
	def getSingleton = H2Resource
	object url extends MappedString(this,H2Constants.url)
	object bytes extends MappedBinary(this)
}
object H2Resource extends H2Resource with LongKeyedMetaMapper[H2Resource]{
}
