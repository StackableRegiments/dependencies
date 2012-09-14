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

  private var highlighters:List[MeTLInk] = List.empty[MeTLInk]
  private var inks:List[MeTLInk] = List.empty[MeTLInk]
  private var images:List[MeTLImage] = List.empty[MeTLImage]
  private var texts:List[MeTLText] = List.empty[MeTLText]
	private var metlMoveDeltas:List[MeTLMoveDelta] = List.empty[MeTLMoveDelta]
  private var quizzes:List[MeTLQuiz] = List.empty[MeTLQuiz]
  private var quizResponses:List[MeTLQuizResponse] = List.empty[MeTLQuizResponse]
  private var submissions:List[MeTLSubmission] = List.empty[MeTLSubmission]
  private var commands:List[MeTLCommand] = List.empty[MeTLCommand]
	private var latestCommands:Map[String,MeTLCommand] = Map.empty[String,MeTLCommand]

	def getLatestCommands:Map[String,MeTLCommand] = latestCommands

  def getHighlighters = highlighters
  def getInks = inks
  def getImages = images
  def getTexts = texts
  def getQuizzes = quizzes
  def getQuizResponses = quizResponses
  def getSubmissions = submissions
  def getCommands = commands

  def getRenderable = Stopwatch.time("History.getRenderable", () => {
    getImages ::: getTexts ::: getHighlighters ::: getInks
  })
	
  def getAll = Stopwatch.time("History.getAll", () => {
    getQuizzes ::: getQuizResponses ::: getSubmissions ::: getCommands ::: getRenderable
  })

	def merge(other:History):History = Stopwatch.time("History.merge", () => {
		val newHistory = History(jid,xScale,yScale,xOffset,yOffset)
		(resetToOriginalVisual.getAll ::: other.resetToOriginalVisual.getAll).sortBy(i => i.timestamp).map(i => newHistory.addStanza(i))
		newHistory
	})	

  def getImageBySource(source:String) = Stopwatch.time("History.getImageBySource", () => getImages.find(i => i.source.map(s => s == source).openOr(false)))
  def getImageByIdentity(identity:String) = Stopwatch.time("History.getImageByIdentity", () => getImages.find(i => i.identity == identity))

  def addStanza(s:MeTLStanza) = Stopwatch.time("History.addStanza", () => {
    s match {
      case s:MeTLDirtyInk => removeHighlighter(s.identity).removeInk(s.identity)
      case s:MeTLDirtyText => removeText(s.identity)
      case s:MeTLDirtyImage => removeImage(s.identity)
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
				removeInk(i.identity)
				if (!s.isDeleted) {
					addInk(i.alterPrivacy(s.newPrivacy).adjustVisual(s.xTranslate,s.yTranslate,s.xScale,s.yScale))
				}
			})
		})
		s.textIds.foreach(id => {
			texts.filter(_.identity == id).map(i => {
				removeText(i.identity)
				if (!s.isDeleted) {
					addText(i.alterPrivacy(s.newPrivacy).adjustVisual(s.xTranslate,s.yTranslate,s.xScale,s.yScale))
				}
			})
		})
		s.imageIds.foreach(id => {
			images.filter(_.identity == id).map(i => {
				removeImage(i.identity)
				if (!s.isDeleted) {
					addImage(i.alterPrivacy(s.newPrivacy).adjustVisual(s.xTranslate,s.yTranslate,s.xScale,s.yScale))
				}
			})
		})
    this
  })
	def addMeTLMoveDelta(s:MeTLMoveDelta) = Stopwatch.time("History.addMeTLMoveDelta", () => {
		val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,xScale,yScale) else s
		metlMoveDeltas = metlMoveDeltas ::: List(newS)
		moveContent(newS)
		update
		this
	})
  def addHighlighter(s:MeTLInk) = Stopwatch.time("History.addHighlighter", () => {
    val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
    highlighters = highlighters ::: List(newS)
    growBounds(newS.left,newS.right,newS.top,newS.bottom)
    update
    this
  })
  def addInk(s:MeTLInk) = Stopwatch.time("History.addInk", () => {
    val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
    inks = inks ::: List(newS)
    growBounds(newS.left,newS.right,newS.top,newS.bottom)
    update
    this
  })
  def addImage(s:MeTLImage) = Stopwatch.time("History.addImage", () => {
    val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
    images = images ::: List(newS)
    growBounds(newS.left,newS.right,newS.top,newS.bottom)
    update
    this
  })
  def addText(s:MeTLText) = Stopwatch.time("History.addText", () => {
    val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
    val (suspectTexts,remainingTexts) = texts.partition(q => q.identity == newS.identity)
    val identifiedTexts = suspectTexts ::: List(newS)
    texts = (identifiedTexts.sortBy(q => q.timestamp).reverse.head) :: remainingTexts
    suspectTexts.headOption match {
      case Some(suspectText) => {
        if ((suspectText.right < newS.right && newS.right > right) || (suspectText.bottom < newS.bottom && newS.bottom > bottom))
          growBounds(newS.left,newS.right,newS.top,newS.bottom)
        else if ((suspectText.right == right && newS.right < right) || (suspectText.bottom == bottom && newS.bottom < bottom))
          calculateBoundsWithout(newS.left,newS.right,newS.top,newS.bottom)
      }
      case None => growBounds(newS.left,newS.right,newS.top,newS.bottom)
    }
    update
    this
  })
  def addQuiz(s:MeTLQuiz) = Stopwatch.time("History.addQuiz", () => {
    val (suspectQuizzes,remainingQuizzes) = quizzes.partition(q => q.id == s.id)
    val newQuiz = (s :: suspectQuizzes).sortBy(q => q.timestamp).reverse.head
    newQuiz.isDeleted match {
      case true => quizzes = remainingQuizzes
      case false => quizzes = newQuiz :: remainingQuizzes
    }
    update
    this
  })
  def addQuizResponse(s:MeTLQuizResponse) = Stopwatch.time("History.addQuizResponse", () => {
    quizResponses = s :: quizResponses
    update
    this
  })
  def addSubmission(s:MeTLSubmission) = Stopwatch.time("History.addSubmission", () => {
    submissions = s :: submissions
    update
    this
  })
  def addCommand(s:MeTLCommand) = Stopwatch.time("History.addCommand", () => {
		latestCommands = latestCommands.updated(s.command,s)
    commands = s :: commands
    update
    this
  })

  def removeHighlighter(identity:String) = Stopwatch.time("History.removeHighlighter", () => {
    val (item,remaining) = highlighters.partition(s => s.identity == identity)
    highlighters = remaining
    item.map(s => calculateBoundsWithout(s.left,s.right,s.top,s.bottom))
    update
    this
  })
  def removeInk(identity:String) = Stopwatch.time("History.removeInk", () => {
    val (item,remaining) = inks.partition(s => s.identity == identity)
    inks = remaining
    item.map(s => calculateBoundsWithout(s.left,s.right,s.top,s.bottom))
    update
    this
  })
  def removeImage(identity:String) = Stopwatch.time("History.removeImage", () => {
    val (item,remaining) = images.partition(s => s.identity == identity)
    images = remaining
    item.map(s => calculateBoundsWithout(s.left,s.right,s.top,s.bottom))
    update
    this
  })
  def removeText(identity:String) = Stopwatch.time("History.removeText", () => {
    val (item,remaining) = texts.partition(s => s.identity == identity)
    texts = remaining
    item.map(s => calculateBoundsWithout(s.left,s.right,s.top,s.bottom))
    update
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
    highlighters.foreach(s => growBounds(s.left,s.right,s.top,s.bottom))
    inks.foreach(s => growBounds(s.left,s.right,s.top,s.bottom))
    images.foreach(s => growBounds(s.left,s.right,s.top,s.bottom))
    texts.foreach(s => growBounds(s.left,s.right,s.top,s.bottom))
  })

  def scale(factor:Double) = Stopwatch.time("History.scale", () => {
    val newHistory = History(jid,factor,factor,0,0)

    highlighters.foreach(s => newHistory.addHighlighter(s))
    inks.foreach(s => newHistory.addInk(s))
    images.foreach(s => newHistory.addImage(s))
    texts.foreach(s => newHistory.addText(s))
    quizzes.foreach(s => newHistory.addQuiz(s))
    quizResponses.foreach(s => newHistory.addQuizResponse(s))
    submissions.foreach(s => newHistory.addSubmission(s))
    commands.foreach(s => newHistory.addCommand(s))

    newHistory
  })
	def resetToOriginalVisual = adjustToVisual(xOffset * -1, yOffset * -1, 1 / xScale, 1 / yScale)
	def adjustToVisual(xT:Double,yT:Double,xS:Double,yS:Double) = Stopwatch.time("History.adjustVisual",() => {
		val newHistory = History(jid,xS * xScale,yS * yScale,xT + xOffset,yT + yOffset)
		highlighters.foreach(s => newHistory.addHighlighter(s))
    inks.foreach(s => newHistory.addInk(s))
    images.foreach(s => newHistory.addImage(s))
    texts.foreach(s => newHistory.addText(s))
    quizzes.foreach(s => newHistory.addQuiz(s))
    quizResponses.foreach(s => newHistory.addQuizResponse(s))
    submissions.foreach(s => newHistory.addSubmission(s))
    commands.foreach(s => newHistory.addCommand(s))
    newHistory
	})
	def getUserSpecificHistory(user:String, isTeacher:Boolean = false) = Stopwatch.time("History.getUserSpecificHistory(%s)".format(user), () => {
		val possiblyAdd = (s:MeTLStanza, action:() => Unit) => {
			if (isTeacher || s.author.toLowerCase == user){
				action()
			}
		}
		val newHistory = History(jid,xScale,yScale,xOffset,yOffset)
		highlighters.foreach(s => possiblyAdd(s,() => newHistory.addHighlighter(s)))
    inks.foreach(s => possiblyAdd(s,() => newHistory.addInk(s)))
    images.foreach(s => possiblyAdd(s,() => newHistory.addImage(s)))
    texts.foreach(s => possiblyAdd(s,() => newHistory.addText(s)))
    quizzes.foreach(s => newHistory.addQuiz(s))
    quizResponses.foreach(s => possiblyAdd(s,() => newHistory.addQuizResponse(s)))
    submissions.foreach(s => possiblyAdd(s,() => newHistory.addSubmission(s)))
    commands.foreach(s => newHistory.addCommand(s))
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

