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
		println("all")
		getAll.foreach(s => println(s))
		println("canvasState")
		getCanvasContents.foreach(s => println(s))
		(resetToOriginalVisual.getAll ::: other.resetToOriginalVisual.getAll).sortBy(i => i.timestamp).map(i => newHistory.addStanza(i))
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

  private def moveContent(s:MeTLMoveDelta) = Stopwatch.time("History.moveContent",()=>{
		getCanvasContents.foreach(cc => moveIndividualContent(s,cc))
		this
  })
	private def moveIndividualContent(s:MeTLMoveDelta,c:MeTLCanvasContent) = Stopwatch.time("History.moveIndividualContent", () => {
		def matches(coll:Seq[String],i:MeTLCanvasContent):Boolean = coll.contains(i.identity) && i.timestamp < s.timestamp && i.privacy == s.privacy
		c match {
			case i:MeTLInk if matches(s.inkIds,i) => {
				removeInk(i.generateDirty(s.timestamp),false)
				if (!s.isDeleted)
					addInk(i.alterPrivacy(s.newPrivacy).adjustVisual(s.xTranslate,s.yTranslate,s.xScale,s.yScale).adjustTimestamp(s.timestamp),false)
			}		
			case i:MeTLText if matches(s.textIds,i) => {
				removeText(i.generateDirty(s.timestamp),false)
				if (!s.isDeleted) 
					addText(i.alterPrivacy(s.newPrivacy).adjustVisual(s.xTranslate,s.yTranslate,s.xScale,s.yScale).adjustTimestamp(s.timestamp),false)
			}
			case i:MeTLImage if matches(s.imageIds,i) => {
				removeImage(i.generateDirty(s.timestamp),false)
				if (!s.isDeleted) 
					addImage(i.alterPrivacy(s.newPrivacy).adjustVisual(s.xTranslate,s.yTranslate,s.xScale,s.yScale).adjustTimestamp(s.timestamp),false)
			}
			case _ => {}
		}
	})
	def addMeTLMoveDelta(s:MeTLMoveDelta,store:Boolean = true) = Stopwatch.time("History.addMeTLMoveDelta", () => {
		val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,xScale,yScale) else s
		moveContent(newS)
		if (store)
			metlMoveDeltas = metlMoveDeltas ::: List(newS)
		update
		this
	})
  def addHighlighter(s:MeTLInk,store:Boolean = true) = Stopwatch.time("History.addHighlighter", () => {
		val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
		if (!dirtyInks.exists(di => di.refersTo(s)) && !metlMoveDeltas.filter(md => md.isDeleted).exists(md => md.refersTo(s))){
			val adjustedInk = metlMoveDeltas.filter(md => !md.isDeleted && md.inkIds.contains(s.identity) && md.privacy == s.privacy).sortBy(_.timestamp).foldLeft(newS)((acc,item) => {
				acc.adjustVisual(item.xTranslate,item.yTranslate,item.xScale,item.yScale).adjustTimestamp(item.timestamp)
			})
			canvasContents = canvasContents.filterNot(cc => cc match {
				case i:MeTLInk => i.identity == s.identity && i.privacy == s.privacy && i.author == s.author
				case _ => false	
			}) ::: List(adjustedInk)
			growBounds(adjustedInk.left,adjustedInk.right,adjustedInk.top,adjustedInk.bottom)
			update
		}
		if (store)
			highlighters = highlighters ::: List(newS)
    this
  })
  def addInk(s:MeTLInk,store:Boolean = true) = Stopwatch.time("History.addInk", () => {
		val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
		if (!dirtyInks.exists(di => di.refersTo(s)) && !metlMoveDeltas.filter(md => md.isDeleted).exists(md => md.refersTo(s))){
			val adjustedInk = metlMoveDeltas.filter(md => !md.isDeleted && md.inkIds.contains(s.identity) && md.privacy == s.privacy).sortBy(_.timestamp).foldLeft(newS)((acc,item) => {
				acc.adjustVisual(item.xTranslate,item.yTranslate,item.xScale,item.yScale).adjustTimestamp(item.timestamp)
			})
			canvasContents = canvasContents.filterNot(cc => cc match {
				case i:MeTLInk => i.identity == s.identity && i.privacy == s.privacy && i.author == s.author
				case _ => false	
			}) ::: List(adjustedInk)
			growBounds(adjustedInk.left,adjustedInk.right,adjustedInk.top,adjustedInk.bottom)
			update
		}
		if (store)
			inks = inks ::: List(newS)
    this
  })
  def addImage(s:MeTLImage,store:Boolean = true) = Stopwatch.time("History.addImage", () => {
		val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
		if (!dirtyImages.exists(di => di.refersTo(s)) && !metlMoveDeltas.filter(md => md.isDeleted).exists(md => md.refersTo(s))){
			val adjustedImage = metlMoveDeltas.filter(md => !md.isDeleted && md.inkIds.contains(s.identity) && md.privacy == s.privacy).sortBy(_.timestamp).foldLeft(newS)((acc,item) => {
				acc.adjustVisual(item.xTranslate,item.yTranslate,item.xScale,item.yScale).adjustTimestamp(item.timestamp)
			})
			canvasContents = canvasContents.filterNot(cc => cc match {
				case i:MeTLImage => i.identity == s.identity && i.privacy == s.privacy && i.author == s.author
				case _ => false
			}) ::: List(adjustedImage)
			growBounds(adjustedImage.left,adjustedImage.right,adjustedImage.top,adjustedImage.bottom)
			update
		}
		if (store)
			images = images ::: List(newS)
    this
  })
  def addText(s:MeTLText,store:Boolean = true) = Stopwatch.time("History.addText", () => {
    val newS = if(shouldAdjust) s.adjustVisual(xOffset,yOffset,1.0,1.0).scale(xScale,yScale) else s
		if (!dirtyTexts.exists(dt => dt.refersTo(s)) && !metlMoveDeltas.filter(md => md.isDeleted).exists(md => md.refersTo(s))){
			val (suspectTexts,remainingContent) = canvasContents.partition(cc => cc match {
				case t:MeTLText => t.identity == s.identity && t.privacy == s.privacy && t.author == s.author
				case _ => false
			})
			val identifiedTexts = (suspectTexts ::: List(newS)).sortBy(q => q.timestamp).reverse
			canvasContents = identifiedTexts.headOption.map(ho => ho match {
				case hot:MeTLText => {
					val adjustedText = metlMoveDeltas.filter(md => !md.isDeleted && md.inkIds.contains(s.identity) && md.privacy == s.privacy).sortBy(_.timestamp).foldLeft(hot)((acc,item) => {
						acc.adjustVisual(item.xTranslate,item.yTranslate,item.xScale,item.yScale).adjustTimestamp(item.timestamp)
					})
					val newCanvasContents = canvasContents ::: List(ho)
					if (adjustedText.left < getLeft || adjustedText.right > getRight || getBottom < adjustedText.bottom || adjustedText.top < getTop)
						growBounds(adjustedText.left,adjustedText.right,adjustedText.top,adjustedText.bottom)
					else if (identifiedTexts.length > 1){
						identifiedTexts(1) match {
						 	case st:MeTLText if ((st.right == getRight && adjustedText.right < getRight) || (st.bottom == getBottom && adjustedText.bottom < getBottom) || (st.top == getTop && adjustedText.top > getTop) || (st.left == getLeft && adjustedText.left > getLeft)) =>
								calculateBoundsWithout(adjustedText.left,adjustedText.right,adjustedText.top,adjustedText.bottom)
							case _ => {}
						}
					}
					newCanvasContents
				}
				case _ => canvasContents
			}).getOrElse(canvasContents)
			update
		}
		if (store)
			texts = texts ::: List(newS)
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
	  if (store)
			dirtyInks = dirtyInks ::: List(dirtyInk)
    this
  })
  def removeImage(dirtyImage:MeTLDirtyImage,store:Boolean = true) = Stopwatch.time("History.removeImage", () => {
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
	  if (store)
			dirtyImages = dirtyImages ::: List(dirtyImage)
    this
  })
  def removeText(dirtyText:MeTLDirtyText,store:Boolean = true) = Stopwatch.time("History.removeText", () => {
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
		if (store)
			dirtyTexts = dirtyTexts ::: List(dirtyText)
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
		top = 0
		left = 0
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
  def shouldRender:Boolean = ((getLeft < 0 || getRight > 0 || getTop < 0 || getBottom > 0) && getCanvasContents.length > 0)
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

