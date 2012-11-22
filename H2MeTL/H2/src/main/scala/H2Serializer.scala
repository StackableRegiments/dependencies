package com.metl.h2

import com.metl.data._
import com.metl.utils._
import com.metl.h2.dbformats._

import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util.Helpers._
import Privacy._

class H2Serializer(configName:String) extends Serializer {
	override type T = Object
	val xmlSerializer = new GenericXmlSerializer(configName)
	val config = ServerConfiguration.configForName(configName)

	case class ParsedCanvasContent(target:String,identity:String,slide:String,privacy:Privacy,author:String,timestamp:Long)
	case class ParsedMeTLContent(author:String,timestamp:Long)

	def toPrivacy(i:String):Privacy = i.toLowerCase.trim match {
		case "public" => Privacy.PUBLIC
		case "private" => Privacy.PRIVATE
		case _ => Privacy.NOT_SET
	}
	def fromPrivacy(i:Privacy):String = i.toString.toLowerCase.trim

	protected def decStanza[A <:H2MeTLStanza[A]](rec:A):ParsedMeTLContent = ParsedMeTLContent(rec.author.is,rec.timestamp.is)
	protected def decCanvasContent[A <: H2MeTLCanvasContent[A]](rec:A):ParsedCanvasContent = {
		val mc = decStanza(rec)
		ParsedCanvasContent(rec.target.is,rec.identity.is,rec.slide.is,toPrivacy(rec.privacy.is),mc.author,mc.timestamp)
	}
	protected def incMeTLContent[A <: H2MeTLContent[A]](rec:A,s:MeTLXml,metlType:String):A = rec.metlType(metlType)
	protected def incStanza[A <: H2MeTLStanza[A]](rec:A,s:MeTLStanza,metlType:String):A = incMeTLContent(rec,s,metlType).timestamp(s.timestamp).author(s.author)		
	protected def incCanvasContent[A <: H2MeTLCanvasContent[A]](rec:A,cc:MeTLCanvasContent,metlType:String):A = incStanza(rec,cc,metlType).asInstanceOf[A].target(cc.target).privacy(fromPrivacy(cc.privacy)).slide(cc.slide).identity(cc.identity)

  def toMeTLStanza[A <: H2MeTLCanvasContent[A]](i:A):MeTLStanza = i.metlType.is match {
		case "ink" => toMeTLInk(i)
		case "text" => toMeTLText(i)
		case "image" => toMeTLImage(i)
		case "dirtyInk" => toMeTLDirtyInk(i)
		case "dirtyText" => toMeTLDirtyText(i)
		case "dirtyImage" => toMeTLDirtyImage(i)
		case "moveDelta" => toMeTLMoveDelta(i)
		case "submission" => toSubmission(i)
		case "command" => toMeTLCommand(i)
		case "quiz" => toMeTLQuiz(i)
		case "quizResponse" => toMeTLQuizResponse(i)
		case _ => throw new SerializationNotImplementedException
	}
  def toMeTLInk(i:H2Ink):MeTLInk = {
		val cc = decCanvasContent(i)
		MeTLInk(config,cc.author,cc.timestamp,i.checksum.is,i.startingSum.is,toPointList(i.points.is),toColor(i.color.is),i.thickness.is,i.isHighlighter.is,cc.target,cc.privacy,cc.slide,cc.identity)
	}
  override def fromMeTLInk(i:MeTLInk):H2Ink = incCanvasContent(H2Ink.create,i,"ink").checksum(i.checksum).startingSum(i.startingSum).points(fromPointList(i.points).toString).color(fromColor(i.color).toString).thickness(i.thickness).isHighlighter(i.isHighlighter)
  def toMeTLImage(i:H2Image):MeTLImage = {
		val cc = decCanvasContent(i)
		val url = i.source.is match {
			case other:String if other.length > 0 => Full(other)
			case _ => Empty
		}
		val imageBytes = url.map(u => config.getResource(u))
		MeTLImage(config,cc.author,cc.timestamp,i.tag.is,url,imageBytes,Empty,i.width.is,i.height.is,i.x.is,i.y.is,cc.target,cc.privacy,cc.slide,cc.identity)
	}
  override def fromMeTLImage(i:MeTLImage):H2Image = incCanvasContent(H2Image.create,i,"image").tag(i.tag).source(i.source.openOr("")).width(i.width).height(i.height).x(i.x).y(i.y)
  def toMeTLText(i:H2Text):MeTLText = {
		val cc = decCanvasContent(i)
		MeTLText(config,cc.author,cc.timestamp,i.text.is,i.height.is,i.width.is,i.caret.is,i.x.is,i.y.is,i.tag.is,i.style.is,i.family.is,i.weight.is,i.size.is,i.decoration.is,cc.identity,cc.target,cc.privacy,cc.slide,toColor(i.color.is))
	}
  override def fromMeTLText(i:MeTLText):H2Text = incCanvasContent(H2Text.create,i,"text").text(i.text).height(i.height).width(i.width).caret(i.caret).x(i.x).y(i.y).tag(i.tag).style(i.style).family(i.family).weight(i.weight).size(i.size).decoration(i.decoration).color(fromColor(i.color).toString)
	def toMeTLMoveDelta(i:H2MoveDelta):MeTLMoveDelta = {
		val cc = decCanvasContent(i)	
		MeTLMoveDelta(config,cc.author,cc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity,stringToStrings(i.inkIds.is),stringToStrings(i.textIds.is),stringToStrings(i.imageIds.is),i.xTranslate.is,i.yTranslate.is,i.xScale.is,i.yScale.is,toPrivacy(i.newPrivacy.is),i.isDeleted.is)
	}
	protected def stringToStrings(s:String):Seq[String] = s.split("_:_")
	protected def stringsToString(ls:Seq[String]):String = ls.foldLeft("")((acc,s) => {
		acc match {
			case other:String if other.length > 0 => "%s_:_%s".format(other,s)
			case _ => s
		}
	})
	override def fromMeTLMoveDelta(i:MeTLMoveDelta):H2MoveDelta = {
		incCanvasContent(H2MoveDelta.create,i,"moveDelta").inkIds(stringsToString(i.inkIds)).textIds(stringsToString(i.textIds)).imageIds(stringsToString(i.imageIds)).xTranslate(i.xTranslate).yTranslate(i.yTranslate).xScale(i.xScale).yScale(i.yScale).newPrivacy(fromPrivacy(i.newPrivacy)).isDeleted(i.isDeleted)
	}
  def toMeTLDirtyInk(i:H2DirtyInk):MeTLDirtyInk = {
		val cc = decCanvasContent(i)
		MeTLDirtyInk(config,cc.author,cc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity)
	}
  override def fromMeTLDirtyInk(i:MeTLDirtyInk):H2DirtyInk = incCanvasContent(H2DirtyInk.create,i,"dirtyInk")
  def toMeTLDirtyImage(i:H2DirtyImage):MeTLDirtyImage = {
		val cc = decCanvasContent(i)
		MeTLDirtyImage(config,cc.author,cc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity)
	}
  override def fromMeTLDirtyImage(i:MeTLDirtyImage):H2DirtyImage = incCanvasContent(H2DirtyImage.create,i,"dirtyImage")
  def toMeTLDirtyText(i:H2DirtyText):MeTLDirtyText = {
		val cc = decCanvasContent(i)
		MeTLDirtyText(config,cc.author,cc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity)
	}
  override def fromMeTLDirtyText(i:MeTLDirtyText):H2DirtyText = incCanvasContent(H2DirtyText.create,i,"dirtyText")
  def toMeTLCommand(i:H2Command):MeTLCommand = {
		val c = decStanza(i)
		
		MeTLCommand(config,c.author,c.timestamp,i.command.is,stringToStrings(i.commandParameters.is).toList)
	}
  override def fromMeTLCommand(i:MeTLCommand):H2Command = incStanza(H2Command.create,i,"command").command(i.command).commandParameters(stringsToString(i.commandParameters))

  def toSubmission(i:H2Submission):MeTLSubmission = {
		val c = decCanvasContent(i)
		val url = i.url.is
		val bytes = config.getResource(url)
		MeTLSubmission(config,c.author,c.timestamp,i.title.is,i.slideJid.is,url,Full(bytes),List.empty[SubmissionBlacklistedPerson],c.target,c.privacy,c.identity)	
	}
  override def fromSubmission(i:MeTLSubmission):H2Submission = incCanvasContent(H2Submission.create,i,"submission").title(i.title).slideJid(i.slideJid).url(i.url)
  def toMeTLQuiz(i:H2Quiz):MeTLQuiz = {
		val url = i.url.is match {
			case s:String if (s.length > 0) => Full(s)
			case _ => Empty
		}
		val bytes = url.map(u => config.getResource(u))
		val c = decStanza(i)
		MeTLQuiz(config,c.author,c.timestamp,i.created.is,i.question.is,i.quizId.is,url,bytes,i.isDeleted.is,optionsFromString(i.options.is))
	}
  override def fromMeTLQuiz(i:MeTLQuiz):H2Quiz = {
		incStanza(H2Quiz.create,i,"quiz").created(i.created).question(i.question).quizId(i.id).url(i.url.openOr("")).isDeleted(i.isDeleted).options(optionsToString(i.options))
	}
  def toMeTLQuizResponse(i:H2QuizResponse):MeTLQuizResponse = {
		val c = decStanza(i)
		MeTLQuizResponse(config,c.author,c.timestamp,i.answer.is,i.answerer.is,i.quizId.is)
	}
  override def fromMeTLQuizResponse(i:MeTLQuizResponse):H2QuizResponse = {
		incStanza(H2QuizResponse.create,i,"quizResponse").answer(i.answer).answerer(i.answerer).quizId(i.id)
	}
  def toConversation(i:H2Conversation):Conversation = Conversation(config,i.author.is,i.lastAccessed.is,slidesFromString(i.slides.is),i.subject.is,i.tag.is,i.jid.is,i.title.is,i.created.is,permissionsFromString(i.permissions.is),stringToStrings(i.blackList.is).toList)	
  override def fromConversation(i:Conversation):H2Conversation = {
		val rec = H2Conversation.find(By(H2Conversation.jid,i.jid)) match {
			case Full(c) => c
			case _ => H2Conversation.create
		}
		incMeTLContent(rec,i,"conversation").author(i.author).lastAccessed(i.lastAccessed).subject(i.subject).tag(i.tag).jid(i.jid).title(i.title).created(i.created).permissions(permissionsToString(i.permissions)).blackList(stringsToString(i.blackList)).slides(slidesToString(i.slides))
	}
	def optionsToString(ls:List[QuizOption]):String = {
		val xml = <options>{ls.map(o => xmlSerializer.fromQuizOption(o))}</options>
		xml.toString
	}
	def optionsFromString(s:String):List[QuizOption] = {
		(scala.xml.XML.loadString(s) \\ "quizOption").map(o => xmlSerializer.toQuizOption(o)).toList
	}
	def slidesToString(ls:List[Slide]):String = {
		"<slides>%s</slides>".format(ls.map(s => xmlSerializer.fromSlide(s).toString).mkString(""))
	}
	def slidesFromString(s:String):List[Slide] = {
		(scala.xml.XML.loadString(s) \\ "slide").map(sl => xmlSerializer.toSlide(sl)).toList
	}
	def permissionsToString(p:Permissions):String = {
		xmlSerializer.fromPermissions(p).toString
	}
	def permissionsFromString(s:String):Permissions = {
		xmlSerializer.toPermissions(scala.xml.XML.loadString(s))
	}
  override def toPointList(input:AnyRef):List[Point] = Stopwatch.time("H2Serializer.toPointList", () => PointConverter.fromText(input.toString))
  override def fromPointList(input:List[Point]):AnyRef = Stopwatch.time("H2Serializer.fromPointList", () => PointConverter.toText(input)) 
  override def toColor(input:AnyRef):Color = ColorConverter.fromARGBHexString(input.toString)
  override def fromColor(input:Color):AnyRef = ColorConverter.toARGBHexString(input)
}
