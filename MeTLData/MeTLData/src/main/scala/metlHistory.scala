package com.metl.data

import com.metl.utils._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import java.util.zip.{ZipInputStream,ZipEntry}
import org.apache.commons.io.IOUtils
import scala.xml.NodeSeq
import java.io.ByteArrayInputStream
import java.util.Date
import Privacy._

case class History(jid:String,xScale:Double = 1.0, yScale:Double = 1.0,xOffset:Double = 0,yOffset:Double = 0) {
  private var lastModifiedTime:Long = 0L
  private def update = lastModifiedTime = new Date().getTime
  def lastModified = lastModifiedTime

	private var stanzas:List[MeTLStanza] = List.empty[MeTLStanza]
	private var canvasContents:List[MeTLCanvasContent] = List.empty[MeTLCanvasContent]
  private var highlighters:List[MeTLInk] = List.empty[MeTLInk]
  private var inks:List[MeTLInk] = List.empty[MeTLInk]
	private var dirtyInks:List[MeTLDirtyInk] = List.empty[MeTLDirtyInk]
  private var images:List[MeTLImage] = List.empty[MeTLImage]
	private var dirtyImages:List[MeTLDirtyImage] = List.empty[MeTLDirtyImage]
  private var texts:List[MeTLText] = List.empty[MeTLText]
	private var dirtyTexts:List[MeTLDirtyText] = List.empty[MeTLDirtyText]
	private var metlMoveDeltas:List[MeTLMoveDelta] = List.empty[MeTLMoveDelta]
  private var quizzes:List[MeTLQuiz] = List.empty[MeTLQuiz]
  private var quizResponses:List[MeTLQuizResponse] = List.empty[MeTLQuizResponse]
  private var submissions:List[MeTLSubmission] = List.empty[MeTLSubmission]
  private var commands:List[MeTLCommand] = List.empty[MeTLCommand]
	private var latestCommands:Map[String,MeTLCommand] = Map.empty[String,MeTLCommand]

	def getLatestCommands:Map[String,MeTLCommand] = latestCommands

	def getAll = stanzas
	def getCanvasContents = canvasContents
  def getHighlighters = highlighters
  def getInks = inks
  def getImages = images
  def getTexts = texts
  def getQuizzes = quizzes
  def getQuizResponses = quizResponses
  def getSubmissions = submissions
  def getCommands = commands

  def getRenderable = Stopwatch.time("History.getRenderable", () => getCanvasContents)

	def getRenderableGrouped:Tuple4[List[MeTLText],List[MeTLInk],List[MeTLInk],List[MeTLImage]] = Stopwatch.time("History.getRenderableGrouped", () => {
		getCanvasContents.foldLeft((List.empty[MeTLText],List.empty[MeTLInk],List.empty[MeTLInk],List.empty[MeTLImage]))((acc,item) => item match {
			case t:MeTLText => (acc._1 ::: List(t),acc._2,acc._3,acc._4)
			case h:MeTLInk if h.isHighlighter => (acc._1,acc._2 ::: List(h),acc._3,acc._4)
			case s:MeTLInk => (acc._1,acc._2,acc._3 ::: List(s),acc._4)
			case i:MeTLImage => (acc._1,acc._2,acc._3,acc._4 ::: List(i))
			case _ => acc
		})
	})
	
	def merge(other:History):History = Stopwatch.time("History.merge", () => {
		val newHistory = History(jid,xScale,yScale,xOffset,yOffset)
		(resetToOriginalVisual.getAll ::: other.resetToOriginalVisual.getAll).map(i => newHistory.addStanza(i))
		newHistory
	})	

  def getImageBySource(source:String) = Stopwatch.time("History.getImageBySource", () => getImages.find(i => i.source.map(s => s == source).openOr(false)))
  def getImageByIdentity(identity:String) = Stopwatch.time("History.getImageByIdentity", () => getImages.find(i => i.identity == identity))

  def addStanza(s:MeTLStanza) = Stopwatch.time("History.addStanza", () => {
    stanzas = stanzas ::: List(s)
		s match {
      case s:MeTLDirtyInk => removeInk(s)
      case s:MeTLDirtyText => removeText(s)
      case s:MeTLDirtyImage => removeImage(s)
      case s:MeTLMoveDelta => addMeTLMoveDelta(s)
      case s:MeTLInk if s.isHighlighter => addHighlighter(s)
      case s:MeTLInk => addInk(s)
      case s:MeTLImage => addImage(s)
      case s:MeTLText => addText(s)
      case s:MeTLQuiz => addQuiz(s)
      case s:MeTLQuizResponse => addQuizResponse(s)
      case s:MeTLSubmission => addSubmission(s)
      case s:MeTLCommand => addCommand(s)
      case s:MeTLStanza => {
        println("makeHistory: I don't know what to do with a MeTLStanza: %s".format(s))
        this
      }
      case _ => this
    }
  })

  def moveContent(s:MeTLMoveDelta) = Stopwatch.time("History.moveContent",()=>{
		s.inkIds.foreach(id => {
			inks.filter(_.identity == id).map(i => {
				removeInk(i.generateDirty,false)
				if (!s.isDeleted) 
					addInk(i.alterPrivacy(s.newPrivacy).adjustVisual(s.xTranslate,s.yTranslate,s.xScale,s.yScale))
			})
		})
		s.textIds.foreach(id => {
			texts.filter(_.identity == id).map(i => {
				removeText(i.generateDirty,false)
				if (!s.isDeleted) 
					addText(i.alterPrivacy(s.newPrivacy).adjustVisual(s.xTranslate,s.yTranslate,s.xScale,s.yScale))
			})
		})
		s.imageIds.foreach(id => {
			images.filter(_.identity == id).map(i => {
				removeImage(i.generateDirty,false)
				if (!s.isDeleted) 
					addImage(i.alterPrivacy(s.newPrivacy).adjustVisual(s.xTranslate,s.yTranslate,s.xScale,s.yScale))
			})
		})
    this
  })
	def addMeTLMoveDelta(s:MeTLMoveDelta,store:Boolean = true) = Stopwatch.time("History.addMeTLMoveDelta", () => {
		val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,xScale,yScale) else s
		if (store)
			metlMoveDeltas = metlMoveDeltas ::: List(newS)
		moveContent(newS)
		update
		this
	})
  def addHighlighter(s:MeTLInk,store:Boolean = true) = Stopwatch.time("History.addHighlighter", () => {
		val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
		if (store)
			highlighters = highlighters ::: List(newS)
		if (!dirtyInks.exists(di => di.refersTo(s))){
			canvasContents = canvasContents ::: List(newS)
			growBounds(newS.left,newS.right,newS.top,newS.bottom)
			update
		}
    this
  })
  def addInk(s:MeTLInk,store:Boolean = true) = Stopwatch.time("History.addInk", () => {
		val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
		if (store)
			inks = inks ::: List(newS)
		if (!dirtyInks.exists(di => di.refersTo(s))){
			canvasContents = canvasContents ::: List(newS)
			growBounds(newS.left,newS.right,newS.top,newS.bottom)
			update
		}
    this
  })
  def addImage(s:MeTLImage,store:Boolean = true) = Stopwatch.time("History.addImage", () => {
		val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
		if (store)
			images = images ::: List(newS)
		if (!dirtyImages.exists(di => di.refersTo(s))){
			canvasContents = canvasContents ::: List(newS)
			growBounds(newS.left,newS.right,newS.top,newS.bottom)
			update
		}
    this
  })
  def addText(s:MeTLText,store:Boolean = true) = Stopwatch.time("History.addText", () => {
    val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
		if (store)
			texts = texts ::: List(newS)
		if (!dirtyTexts.exists(dt => dt.refersTo(s))){
			val (suspectTexts,remainingContent) = canvasContents.partition(cc => cc match {
				case t:MeTLText => t.identity == s.identity && t.privacy == s.privacy 
				case _ => false
			})
			val identifiedTexts = suspectTexts ::: List(newS)
			canvasContents = identifiedTexts.sortBy(q => q.timestamp).reverse.headOption.map(ho => canvasContents ::: List(ho)).getOrElse(canvasContents) 
			suspectTexts.headOption match {
				case Some(suspectText) => suspectText match {
					case st:MeTLText => {
						if ((st.right < newS.right && newS.right > right) || (st.bottom < newS.bottom && newS.bottom > bottom))
							growBounds(newS.left,newS.right,newS.top,newS.bottom)
						else if ((st.right == right && newS.right < right) || (st.bottom == bottom && newS.bottom < bottom))
							calculateBoundsWithout(newS.left,newS.right,newS.top,newS.bottom)
					}
					case _ => growBounds(newS.left,newS.right,newS.top,newS.bottom)
				}
				case None => growBounds(newS.left,newS.right,newS.top,newS.bottom)
			}
			update
		}
    this
  })
  def addQuiz(s:MeTLQuiz,store:Boolean = true) = Stopwatch.time("History.addQuiz", () => {
	  if (store){
			val (suspectQuizzes,remainingQuizzes) = quizzes.partition(q => q.id == s.id)
			val newQuiz = (s :: suspectQuizzes).sortBy(q => q.timestamp).reverse.head
			newQuiz.isDeleted match {
				case true => quizzes = remainingQuizzes
				case false => quizzes = newQuiz :: remainingQuizzes
			}
			update
		}
    this
  })
  def addQuizResponse(s:MeTLQuizResponse,store:Boolean = true) = Stopwatch.time("History.addQuizResponse", () => {
	  if (store)
			quizResponses = quizResponses ::: List(s)
    update
    this
  })
  def addSubmission(s:MeTLSubmission,store:Boolean = true) = Stopwatch.time("History.addSubmission", () => {
	  if (store)
			submissions = submissions ::: List(s)
    update
    this
  })
  def addCommand(s:MeTLCommand,store:Boolean = true) = Stopwatch.time("History.addCommand", () => {
	  if (store)
			latestCommands = latestCommands.updated(s.command,s)
    commands = commands ::: List(s)
    update
    this
  })
  def removeInk(dirtyInk:MeTLDirtyInk,store:Boolean = true) = Stopwatch.time("History.removeInk", () => {
	  if (store)
			dirtyInks = dirtyInks ::: List(dirtyInk)
		if (inks.exists(i => dirtyInk.refersTo(i))){
			val (item,remaining) = canvasContents.partition(s => s match {
				case i:MeTLInk => dirtyInk.refersTo(i)
				case _ => false
			})
			canvasContents = remaining
			item.map(s => s match {
				case i:MeTLInk => calculateBoundsWithout(i.left,i.right,i.top,i.bottom)
				case _ => {}
			})
			update
		}
    this
  })
  def removeImage(dirtyImage:MeTLDirtyImage,store:Boolean = true) = Stopwatch.time("History.removeImage", () => {
	  if (store)
			dirtyImages = dirtyImages ::: List(dirtyImage)
		if (images.exists(i => dirtyImage.refersTo(i))){
			val (item,remaining) = canvasContents.partition(s => s match {
					case i:MeTLImage => dirtyImage.refersTo(i)
					case _ => false
				}
			)
			canvasContents = remaining
			item.map(s => s match {
				case i:MeTLImage => calculateBoundsWithout(i.left,i.right,i.top,i.bottom)
				case _ => {}
			})
			update
		}
    this
  })
  def removeText(dirtyText:MeTLDirtyText,store:Boolean = true) = Stopwatch.time("History.removeText", () => {
		if (store)
			dirtyTexts = dirtyTexts ::: List(dirtyText)
		if (texts.exists(i => dirtyText.refersTo(i))){
			val (item,remaining) = canvasContents.partition(s => s match {
				case t:MeTLText => dirtyText.refersTo(t)
				case _ => false
			})
			canvasContents = remaining
			item.map(s => s match {
				case t:MeTLText => calculateBoundsWithout(t.left,t.right,t.top,t.bottom)
				case _ => {}
			})
			update
		}
    this
  })

	private var left:Double = 0
  private var right:Double = 0
	private var top:Double = 0
  private var bottom:Double = 0

	def getLeft = left
  def getRight = right
	def getTop = top
  def getBottom = bottom
	

  private def growBounds(sLeft:Double,sRight:Double,sTop:Double,sBottom:Double) = Stopwatch.time("History.growBounds", () => {
		if (!sLeft.isNaN)
			left = Math.min(left,sLeft)
    if (!sRight.isNaN)
      right = Math.max(right,sRight)
		if (!sTop.isNaN)
			top = Math.min(top,sTop)
    if (!sBottom.isNaN)
      bottom = Math.max(bottom,sBottom)
  })

  private def calculateBoundsWithout(sLeft:Double,sRight:Double,sTop:Double,sBottom:Double) = Stopwatch.time("History.calculateBoundsWithout", () => {
    if (sLeft == left || sRight == right || sTop == top || sBottom == bottom)
      calculateBounds
  })

  private def calculateBounds = Stopwatch.time("History.calculateBounds", () => {
    right = 0
    bottom = 0
		getCanvasContents.foreach(s => s match {
			case i:MeTLInk => growBounds(i.left,i.right,i.top,i.bottom)
			case i:MeTLImage => growBounds(i.left,i.right,i.top,i.bottom)
			case t:MeTLText => growBounds(t.left,t.right,t.top,t.bottom)
		})
  })

  def scale(factor:Double) = Stopwatch.time("History.scale", () => {
    val newHistory = History(jid,factor,factor,0,0)
		getAll.foreach(i => newHistory.addStanza(i))
	  newHistory
  })
	def resetToOriginalVisual = adjustToVisual(xOffset * -1, yOffset * -1, 1 / xScale, 1 / yScale)
	def adjustToVisual(xT:Double,yT:Double,xS:Double,yS:Double) = Stopwatch.time("History.adjustVisual",() => {
		val newHistory = History(jid,xS * xScale,yS * yScale,xT + xOffset,yT + yOffset)
		getAll.foreach(i => newHistory.addStanza(i))
    newHistory
	})
	def getUserSpecificHistory(user:String, isTeacher:Boolean = false) = Stopwatch.time("History.getUserSpecificHistory(%s)".format(user), () => {
		val possiblyAdd = (s:MeTLStanza, action:() => Unit) => {
			if (isTeacher || s.author.toLowerCase == user)
				action()
		}
		val newHistory = History(jid,xScale,yScale,xOffset,yOffset)
		getAll.foreach(i => i match {
			case q:MeTLQuiz => newHistory.addQuiz(q)
			case c:MeTLCommand => newHistory.addCommand(c)
			case s:MeTLStanza => possiblyAdd(s,() => newHistory.addStanza(s))
		})
    newHistory
	})
	private def shouldAdjust:Boolean = (xScale != 1.0 || yScale != 1.0 || xOffset != 0 || yOffset != 0)
  def shouldRender:Boolean = ((getLeft < 0 || getRight > 0 || getTop < 0 || getBottom > 0) && (highlighters.length > 0 || inks.length > 0 || texts.length > 0 || images.length > 0))
}

object History {
  def empty = History("")
}

abstract class HistoryRetriever(serverName:String) {
  lazy val server = ServerConfiguration.configForName(serverName)
  def getMeTLHistory(jid:String):History
  def makeHistory(jid:String,stanzas:List[MeTLStanza]):History = Stopwatch.time("History.makeHistory", () => {
    stanzas.sortBy(s => s.timestamp).foldLeft(new History(jid))((h,item) => h.addStanza(item))
  })
}

object EmptyHistory extends HistoryRetriever("empty") {
  def getMeTLHistory(jid:String) = History.empty
}

