package com.metl.data

import com.metl.utils._

import net.liftweb.common._
import net.liftweb.util.Helpers._
import java.util.Date

object PointConverter {
  def fromText(t:String):List[Point] = parsePoints(t.split(" ").toList)
  def toText(points:List[Point]):String = points.map(p => toText(p)).mkString(" ")
  def toText(point:Point):String = "%s %s %s".format(point.x,point.y,point.thickness)
  private def constructPoint(pointElements:List[String]):Point = {
    if (pointElements.length == 3){
      val x = tryo(pointElements(0).toDouble).openOr(0.0)
      val y = tryo(pointElements(1).toDouble).openOr(0.0)
      val thickness = tryo(pointElements(2).toDouble).openOr(0.0)
      Point(x,y,thickness)
    }
    else Point.empty
  }
  private def parsePoints(incomingPoints:List[String]):List[Point] = {
    incomingPoints.splitAt(3) match {
      case (currentPoint,remainingPoints) if (currentPoint.length == 3) => constructPoint(currentPoint) :: parsePoints(remainingPoints)
      case (lastPoint,List()) if (lastPoint.length == 3) => List(constructPoint(lastPoint))
      case _ => List.empty[Point]
    }
  }
}

object ColorConverter{
  def toHexString(c:Color) = toARGBHexString(c)
  def toRGBHexString(c:Color) = "#%02x%02x%02x".format(c.red,c.green,c.blue)
  def toARGBHexString(c:Color) = "#%02x%02x%02x%02x".format(c.alpha,c.red,c.green,c.blue)
  def toRGBAString(c:Color) = "%s %s %s %s".format(c.red,c.green,c.blue,c.alpha)
  def fromText:PartialFunction[String,Color] = {
    case s:String if (s.length == 9 && s.startsWith("#")) => fromHexString(s)
    case s:String if (s.split(" ").length == 3) => asSplit(s+ " 255")
    case s:String if (s.split(" ").length == 4) => asSplit(s)
    case _ => Color.default
  }
  private def hexToInt(h:String):Int = tryo(Integer.parseInt(h,16)).openOr(0)
  private       def convert2AfterN(h:String,n:Int):Int = hexToInt(h.drop(n).take(2).mkString)
  def fromHexString(h:String):Color = fromARGBHexString(h)
  def fromRGBHexString(h:String):Color = {
    val r = convert2AfterN(h,1)
    val g = convert2AfterN(h,3)
    val b = convert2AfterN(h,5)
    Color(255,clamp(r),clamp(g),clamp(b))
  }
  def fromARGBHexString(h:String):Color = {
    val a = convert2AfterN(h,1)
    val r = convert2AfterN(h,3)
    val g = convert2AfterN(h,5)
    val b = convert2AfterN(h,7)
    Color(clamp(a),clamp(r),clamp(g),clamp(b))
  }
  def asSplit(l:String):Color = {
    val parts = l.split(" ").map(_.toInt).toList
    val a = parts(3)
    val r = parts(0)
    val g = parts(1)
    val b = parts(2)
    Color(clamp(a),clamp(r),clamp(g),clamp(b))
  }
  private def clamp (n:Integer,min:Integer=0,max:Integer=255) = Math.max(min,Math.min(max,n))
}

object Privacy extends Enumeration{
  type Privacy = Value
  val PUBLIC, PRIVATE, NOT_SET = Value
  def parse(possiblePrivacy:String):Privacy = possiblePrivacy match {
    case s:String if s.toLowerCase == "public" => PUBLIC
    case s:String if s.toLowerCase == "private" => PRIVATE
		case s:String if s.toLowerCase == "not_set" => NOT_SET
    case _ => NOT_SET
  }
}
import Privacy._

case class Color(alpha:Int,red:Int,green:Int,blue:Int)
object Color{
  def empty = Color(0,0,0,0)
  def default:Color = Color(255,255,255,255)
}
case class Point(x:Double,y:Double,thickness:Double){
}
object Point{
  val empty = Point(0.0,0.0,0.0)
}
case class Presentation(override val server:ServerConfiguration,conversation:Conversation,stanzas:Map[Int,List[MeTLStanza]] = Map.empty[Int,List[MeTLStanza]],metaData:List[Tuple2[String,String]] = List.empty[Tuple2[String,String]]) extends MeTLXml(server)
object Presentation{
  def emtpy = Presentation(ServerConfiguration.empty,Conversation.empty)
}

case class Conversation(override val server:ServerConfiguration,author:String,lastAccessed:Long,slides:List[Slide],subject:String,tag:String,jid:Int,title:String,created:String,permissions:Permissions, blackList:List[String] = List.empty[String]) extends MeTLXml(server)
object Conversation{
  def empty = Conversation(ServerConfiguration.empty,"",0L,List.empty[Slide],"","",0,"","",Permissions.default(ServerConfiguration.empty))
}

case class Slide(override val server:ServerConfiguration,author:String,id:Int,index:Int,defaultHeight:Int = 540, defaultWidth:Int = 720, exposed:Boolean = false, slideType:String = "SLIDE") extends MeTLXml(server)
object Slide{
  def empty = Slide(ServerConfiguration.empty,"",0,0)
}

case class Permissions(override val server:ServerConfiguration, studentsCanOpenFriends:Boolean,studentsCanPublish:Boolean,usersAreCompulsorilySynced:Boolean) extends MeTLXml(server)
object Permissions{
  def empty = Permissions(ServerConfiguration.empty,false,false,false)
  def default(server:ServerConfiguration = ServerConfiguration.default) = Permissions(server,false,true,false)
}

case class MeTLXml(server:ServerConfiguration)
object MeTLXml {
  def empty = MeTLXml(ServerConfiguration.empty)
}

case class MeTLStanza(override val server:ServerConfiguration,author:String,timestamp:Long) extends MeTLXml(server)
object MeTLStanza{
  def empty = MeTLStanza(ServerConfiguration.empty,"",0L)
}

case class MeTLCanvasContent(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,target:String,privacy:Privacy,slide:String,identity:String,scaleFactorX:Double = 1.0,scaleFactorY:Double = 1.0) extends MeTLStanza(server,author,timestamp)

case class MeTLInk(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,checksum:Double,startingSum:Double,points:List[Point],color:Color,thickness:Double,isHighlighter:Boolean,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val scaleFactorX:Double = 1.0,override val scaleFactorY:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,scaleFactorX,scaleFactorY) {
  private def offsetAt(point:Point) = point.thickness/256*thickness
  val left:Double = points.map(p => p.x-offsetAt(p)).min
  val right:Double = points.map(p => p.x+offsetAt(p)).max
  val top:Double = points.map(p => p.y-offsetAt(p)).min
  val bottom:Double = points.map(p => p.y+offsetAt(p)).max
	def scale(factor:Double):MeTLInk = scale(factor,factor)
  def scale(xScale:Double,yScale:Double):MeTLInk = Stopwatch.time("MeTLInk.scale", () => {
		val averageFactor = (xScale + yScale) / 2
    MeTLInk(server,author,timestamp,checksum,startingSum,points.map(p => Point(p.x*xScale,p.y*yScale,p.thickness)),color,thickness*averageFactor,isHighlighter,target,privacy,slide,identity,scaleFactorX * xScale,scaleFactorY * yScale)
  })
	def alterPrivacy(newPrivacy:Privacy):MeTLInk = Stopwatch.time("MeTLInk.alterPrivacy", () => {
		newPrivacy match {
			case p:Privacy if (p == privacy) => this
			case Privacy.NOT_SET => this
			case p:Privacy => MeTLInk(server,author,timestamp,checksum,startingSum,points,color,thickness,isHighlighter,target,p,slide,identity,scaleFactorX,scaleFactorY)
			case _ => this
		}
	})
	def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double) = Stopwatch.time("MeTLInk.adjustVisual", () => {
			val averageFactor = (xScale + yScale) / 2
			val newPoints = (xTranslate,yTranslate,xScale,yScale) match {
				case (0,0,1.0,1.0) => points
				case (xO,yO,1.0,1.0) => points.map(p => Point(p.x+xO,p.y+yO,p.thickness))
				case (0,0,xS,yS) => points.map(p => Point((((p.x - left) * xS) + left),(((p.y - top) * yS) + top),p.thickness)) 
				case (xO,yO,xS,yS) => points.map(p => Point((((p.x - left) * xS) + left + xO),(((p.y - top) * yS) + top + yO),p.thickness))
			}
			MeTLInk(server,author,timestamp,checksum,startingSum,newPoints,color,thickness * averageFactor,isHighlighter,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
	})
	def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLInk = Stopwatch.time("MeTLInk.adjustTimestamp", () => {
			MeTLInk(server,author,newTime,checksum,startingSum,points,color,thickness,isHighlighter,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
	})
	def generateDirty(dirtyTime:Long = new java.util.Date().getTime):MeTLDirtyInk = Stopwatch.time("MeTLInk.generateDirty", () => {
			MeTLDirtyInk(server,author,dirtyTime,target,privacy,slide,identity)
	})
}

object MeTLInk{
  def empty = MeTLInk(ServerConfiguration.empty,"",0L,0.0,0.0,List.empty[Point],Color.default,0.0,false,"",Privacy.NOT_SET,"","")
}

case class MeTLImage(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,tag:String,source:Box[String],imageBytes:Box[Array[Byte]],pngBytes:Box[Array[Byte]],width:Double,height:Double,x:Double,y:Double,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val scaleFactorX:Double = 1.0,override val scaleFactorY:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,scaleFactorX,scaleFactorY) {
  val left:Double = x
  val right:Double = x+width
  val top:Double = y
  val bottom:Double = y+height
	def scale(factor:Double):MeTLImage = scale(factor,factor)
  def scale(xScale:Double,yScale:Double):MeTLImage = Stopwatch.time("MeTLImage.scale", () => {
    MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width*xScale,height*yScale,x*xScale,y*yScale,target,privacy,slide,identity,scaleFactorX * xScale,scaleFactorY * yScale)
  })
	def alterPrivacy(possiblyNewPrivacy:Privacy):MeTLImage = Stopwatch.time("MeTLImage.alterPrivacy", () => {
		possiblyNewPrivacy match {
			case p:Privacy if (p == privacy) => this
			case Privacy.NOT_SET => this
			case p:Privacy => MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,target,p,slide,identity,scaleFactorX,scaleFactorY)
			case _ => this
		}
	})
	def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double):MeTLImage = Stopwatch.time("MeTLImage.adjustVisual", () => {
		MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width * xScale,height * yScale,x + xTranslate,y + yTranslate,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
	})
	def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLImage = Stopwatch.time("MeTLimage.adjustTimestamp", () => {
		MeTLImage(server,author,newTime,tag,source,imageBytes,pngBytes,width,height,x,y,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
	})
	def generateDirty(dirtyTime:Long = new java.util.Date().getTime):MeTLDirtyImage = Stopwatch.time("MeTLImage.generateDirty", () => {
		MeTLDirtyImage(server,author,dirtyTime,target,privacy,slide,identity)
	})
}

object MeTLImage{
  def empty = MeTLImage(ServerConfiguration.empty,"",0L,"",Empty,Empty,Empty,0.0,0.0,0.0,0.0,"",Privacy.NOT_SET,"","")
}

case class MeTLText(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,text:String,height:Double,width:Double,caret:Int,x:Double,y:Double,tag:String,style:String,family:String,weight:String,size:Double,decoration:String,override val identity:String,override val target:String,override val privacy:Privacy,override val slide:String,color:Color,override val scaleFactorX:Double = 1.0,override val scaleFactorY:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,scaleFactorX,scaleFactorY) {
  def left:Double = x
  def right:Double = x+width
  def top:Double = y
  def bottom:Double = y+height
	def scale(factor:Double):MeTLText = scale(factor,factor)
  def scale(xScale:Double,yScale:Double):MeTLText = Stopwatch.time("MeTLText.scale", () => {
		val averageFactor = (xScale + yScale) / 2
    MeTLText(server,author,timestamp,text,height*yScale,width*xScale,caret,x*xScale,y*yScale,tag,style,family,weight,size*averageFactor,decoration,identity,target,privacy,slide,color,scaleFactorX * xScale,scaleFactorY * yScale)
  })
	def alterPrivacy(possiblyNewPrivacy:Privacy):MeTLText = Stopwatch.time("MeTLText.alterPrivacy", () => {
		possiblyNewPrivacy match {
			case p:Privacy if (p == privacy) => this
			case Privacy.NOT_SET => this
			case p:Privacy => MeTLText(server,author,timestamp,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,identity,target,p,slide,color,scaleFactorX,scaleFactorY)
			case _ => this
		}
	})
	def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double):MeTLText = Stopwatch.time("MeTLText.adjustVisual", () => {
		val averageFactor = (xScale + yScale) / 2
		MeTLText(server,author,timestamp,text,height * yScale,width * xScale,caret,x + xTranslate,y + yTranslate,tag,style,family,weight,size * averageFactor,decoration,identity,target,privacy,slide,color,scaleFactorX,scaleFactorY)
	})
	def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLText = Stopwatch.time("MeTLText.adjustTimestamp", () => {
		MeTLText(server,author,newTime,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,identity,target,privacy,slide,color,scaleFactorX,scaleFactorY)	
	})
	def generateDirty(dirtyTime:Long = new java.util.Date().getTime):MeTLDirtyText = Stopwatch.time("MeTLText.generateDirty", () => {
		MeTLDirtyText(server,author,dirtyTime,target,privacy,slide,identity)
	})
}

object MeTLText{
  def empty = MeTLText(ServerConfiguration.empty,"",0L,"",0.0,0.0,0,0.0,0.0,"","","","",0.0,"","","",Privacy.NOT_SET,"",Color.default)
}

case class MeTLMoveDelta(override val server:ServerConfiguration, override val author:String,override val timestamp:Long,override val target:String, override val privacy:Privacy,override val slide:String,override val identity:String,inkIds:Seq[String],textIds:Seq[String],imageIds:Seq[String],xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double,newPrivacy:Privacy,isDeleted:Boolean) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity){
	def generateDirtier(newInkIds:Seq[String],newTextIds:Seq[String],newImageIds:Seq[String],replacementPrivacy:Privacy):MeTLMoveDelta = Stopwatch.time("MeTLMoveDelta.generateDirtier", () => {
		MeTLMoveDelta(server,author,timestamp,target,replacementPrivacy,slide,identity,newInkIds,newTextIds,newImageIds,0.0,0.0,1.0,1.0,Privacy.NOT_SET,true)
	})
	def replaceIds(newInkIds:Seq[String],newTextIds:Seq[String],newImageIds:Seq[String],replacementPrivacy:Privacy):MeTLMoveDelta = Stopwatch.time("MeTLMoveDelta.replaceIds", () => {
		MeTLMoveDelta(server,author,timestamp,target,replacementPrivacy,slide,identity,newInkIds,newTextIds,newImageIds,xTranslate,yTranslate,xScale,yScale,Privacy.NOT_SET,isDeleted)
	})
	def adjustVisual(newXTranslate:Double,newYTranslate:Double,newXScale:Double,newYScale:Double):MeTLMoveDelta = {
		MeTLMoveDelta(server,author,timestamp,target,privacy,slide,identity,inkIds,textIds,imageIds,xTranslate + newXTranslate,yTranslate + newYTranslate,xScale * newXScale,yScale * newYScale,newPrivacy,isDeleted)
	}
	def scale(newXScale:Double,newYScale:Double):MeTLMoveDelta = {
		MeTLMoveDelta(server,author,timestamp,target,privacy,slide,identity,inkIds,textIds,imageIds,xTranslate * newXScale,yTranslate * newYScale,xScale * newXScale,yScale * newYScale,newPrivacy,isDeleted)
	}
	def adjustIndividualContent(cc:MeTLCanvasContent):MeTLCanvasContent = {
		cc match {
			case i:MeTLInk => i.adjustVisual(xTranslate,yTranslate,xScale,yScale).adjustTimestamp(timestamp).alterPrivacy(newPrivacy)
			case t:MeTLText => t.adjustVisual(xTranslate,yTranslate,xScale,yScale).adjustTimestamp(timestamp).alterPrivacy(newPrivacy)
			case i:MeTLImage => i.adjustVisual(xTranslate,yTranslate,xScale,yScale).adjustTimestamp(timestamp).alterPrivacy(newPrivacy)
			case _ => cc
		}
	}
	def generateChanges(publicHistory:History,privateHistory:History):Tuple2[List[MeTLStanza],List[MeTLStanza]] = Stopwatch.time("MeTLMoveDelta.generateChanges", () => {
		val privateInks = privateHistory.getInks.filter(i => inkIds.contains(i.identity))
		val privateHighlighters = privateHistory.getHighlighters.filter(i => inkIds.contains(i.identity))
		val privateTexts = privateHistory.getTexts.filter(i => textIds.contains(i.identity))
		val privateImages = privateHistory.getImages.filter(i => imageIds.contains(i.identity))
		val publicInks = publicHistory.getInks.filter(i => inkIds.contains(i.identity))
		val publicHighlighters = publicHistory.getHighlighters.filter(i => inkIds.contains(i.identity))
		val publicTexts = publicHistory.getTexts.filter(i => textIds.contains(i.identity))
		val publicImages = publicHistory.getImages.filter(i => imageIds.contains(i.identity))
		newPrivacy match {				
			case p:Privacy if p == Privacy.PUBLIC => {
				val notP = Privacy.PRIVATE
				val privateInksToPublicize = privateInks.map(i => adjustIndividualContent(i))
				val privateHighlightersToPublicize = privateHighlighters.map(i => adjustIndividualContent(i))
				val privateTextsToPublicize = privateTexts.map(i => adjustIndividualContent(i)) 
				val privateImagesToPublicize = privateImages.map(i => adjustIndividualContent(i)) 
				val privateDirtier = ((privateInksToPublicize ::: privateHighlightersToPublicize ::: privateTextsToPublicize ::: privateImagesToPublicize).length > 0) match {
					case true => List(generateDirtier(privateInksToPublicize.map(i => i.identity) ::: privateHighlightersToPublicize.map(i => i.identity),privateTextsToPublicize.map(i => i.identity),privateImagesToPublicize.map(i => i.identity),notP))
					case _ => List.empty[MeTLStanza]
				}
				val publicAdjuster = ((publicInks ::: publicHighlighters ::: publicTexts ::: publicImages).length > 0) match {
					case true => List(replaceIds(publicInks.map(i=>i.identity) ::: publicHighlighters.map(i => i.identity),publicTexts.map(i=>i.identity),publicImages.map(i=>i.identity),p))
					case _ => List.empty[MeTLStanza]
				}
				(publicAdjuster ::: privateInksToPublicize ::: privateTextsToPublicize ::: privateImagesToPublicize, privateDirtier)
			}
			case p:Privacy if p == Privacy.PRIVATE => {
				val notP = Privacy.PUBLIC
				val publicInksToPrivatize = publicInks.map(i => adjustIndividualContent(i))
				val publicHighlightersToPrivatize = publicHighlighters.map(i => adjustIndividualContent(i))
				val publicTextsToPrivatize = publicTexts.map(i => adjustIndividualContent(i))
				val publicImagesToPrivatize = publicImages.map(i => adjustIndividualContent(i))
				val publicDirtiers = ((publicInksToPrivatize ::: publicTextsToPrivatize ::: publicImagesToPrivatize).length > 0) match {
					case true => List(generateDirtier(publicInksToPrivatize.map(i => i.identity) ::: publicHighlightersToPrivatize.map(i => i.identity),publicTextsToPrivatize.map(i => i.identity),publicImagesToPrivatize.map(i => i.identity),notP))
					case _ => List.empty[MeTLStanza]
				}
				val privateAdjusters = ((privateInks ::: privateHighlighters ::: privateTexts ::: privateImages).length > 0) match {
					case true => List(replaceIds(privateInks.map(i => i.identity) ::: privateHighlighters.map(i => i.identity),privateTexts.map(i => i.identity),privateImages.map(i => i.identity),p))
					case _ => List.empty[MeTLStanza]
				}
				(publicDirtiers,privateAdjusters ::: publicInksToPrivatize ::: publicHighlightersToPrivatize ::: publicTextsToPrivatize ::: publicImagesToPrivatize)
			}
			case _ => {
				val privDelta = ((privateInks ::: privateHighlighters ::: privateTexts ::: privateImages).length > 0) match {
					case true => List(replaceIds(privateInks.map(i=>i.identity) ::: privateHighlighters.map(i => i.identity),privateTexts.map(i=>i.identity),privateImages.map(i=>i.identity),Privacy.PRIVATE))
					case _ => List.empty[MeTLStanza]
				}
				val pubDelta = ((publicInks ::: publicHighlighters ::: publicTexts ::: publicImages).length > 0) match {
					case true => List(replaceIds(publicInks.map(i=>i.identity) ::: publicHighlighters.map(i => i.identity),publicTexts.map(i=>i.identity),publicImages.map(i=>i.identity),Privacy.PUBLIC))
					case _ => List.empty[MeTLStanza]
				}
				(pubDelta,privDelta)
			}
		}
	})
	def refersTo(other:MeTLCanvasContent):Boolean = {
		(inkIds.contains(other.identity) || textIds.contains(other.identity) || imageIds.contains(other.identity)) && privacy == other.privacy && timestamp > other.timestamp
	}
}
case object MeTLMoveDelta{ 
  def empty = MeTLMoveDelta(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","",Nil,Nil,Nil,0.0,0.0,1.0,1.0,Privacy.NOT_SET,false)
}

case class MeTLMove(override val server:ServerConfiguration, override val author:String,override val timestamp:Long,override val target:String, override val privacy:Privacy,override val slide:String,override val identity:String,inks:Seq[MeTLInk],texts:Seq[MeTLText],images:Seq[MeTLImage]) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity)
case object MeTLMove{ 
  def empty = MeTLMove(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","",Nil,Nil,Nil)
}

case class MeTLDirtyInk(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity) {
	def alterPrivacy(newPrivacy:Privacy):MeTLDirtyInk = MeTLDirtyInk(server,author,timestamp,target,newPrivacy,slide,identity)
	def refersTo(other:MeTLInk):Boolean = identity == other.identity && privacy == other.privacy && timestamp > other.timestamp
}
object MeTLDirtyInk{
  def empty = MeTLDirtyInk(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","")
}

case class MeTLDirtyText(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity){
	def alterPrivacy(newPrivacy:Privacy):MeTLDirtyText = MeTLDirtyText(server,author,timestamp,target,newPrivacy,slide,identity)
	def refersTo(other:MeTLText):Boolean = identity == other.identity && privacy == other.privacy && timestamp > other.timestamp
}
object MeTLDirtyText{
  def empty = MeTLDirtyText(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","")
}

case class MeTLDirtyImage(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity) {
	def alterPrivacy(newPrivacy:Privacy):MeTLDirtyImage = MeTLDirtyImage(server,author,timestamp,target,newPrivacy,slide,identity)
	def refersTo(other:MeTLImage):Boolean = identity == other.identity && privacy == other.privacy && timestamp > other.timestamp
}
object MeTLDirtyImage{
  def empty = MeTLDirtyImage(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","")
}

case class MeTLCommand(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,command:String,commandParameters:List[String]) extends MeTLStanza(server,author,timestamp)
object MeTLCommand{
  def empty = MeTLCommand(ServerConfiguration.empty,"",0L,"/No Command",List.empty[String])
}

case class MeTLQuiz(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,created:Long,question:String,id:String,url:Box[String],imageBytes:Box[Array[Byte]],isDeleted:Boolean,options:List[QuizOption]) extends MeTLStanza(server,author,timestamp)
object MeTLQuiz{
  def empty = MeTLQuiz(ServerConfiguration.empty,"",0L,0L,"","",Empty,Empty,true,List.empty[QuizOption])
}

case class MeTLSubmission(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,title:String,slideJid:Int,url:String,blacklist:List[SubmissionBlacklistedPerson] = List.empty[SubmissionBlacklistedPerson], override val target:String = "submission",override val privacy:Privacy = Privacy.PUBLIC,override val identity:String = new Date().getTime.toString) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slideJid.toString,identity)
object MeTLSubmission{
  def empty = MeTLSubmission(ServerConfiguration.empty,"",0L,"",0,"")
}
case class SubmissionBlacklistedPerson(username:String,highlight:Color)
object SubmissionBlacklistedPerson{
	def empty = SubmissionBlacklistedPerson("",Color.default)
}

case class QuizOption(name:String,text:String,correct:Boolean,color:Color)
object QuizOption{
  def empty = QuizOption("","",false,Color.default)
}

case class MeTLQuizResponse(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,answer:String,answerer:String,id:String) extends MeTLStanza(server,author,timestamp)
object MeTLQuizResponse{
  def empty = MeTLQuizResponse(ServerConfiguration.empty,"",0L,"","","")
}
