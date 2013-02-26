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
    case s:String if (s.startsWith("Color(")) => {
      asSplit(s.drop("Color(".length).takeWhile(c => c != ')').split(',').drop(1).mkString(" ") + " 255")

    }
    case s => {
      Color.default
    }
  }
  private def hexToInt(h:String):Int = tryo(Integer.parseInt(h,16)).openOr(0)
  private def convert2AfterN(h:String,n:Int):Int = hexToInt(h.drop(n).take(2).mkString)
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
  private def asSplit(l:String):Color = {
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

case class Conversation(override val server:ServerConfiguration,author:String,lastAccessed:Long,slides:List[Slide],subject:String,tag:String,jid:Int,title:String,created:String,permissions:Permissions, blackList:List[String] = List.empty[String]) extends MeTLXml(server){
	def delete = Conversation(server,author,new Date().getTime,slides,"deleted",tag,jid,title,created,permissions,blackList)
	def rename(newTitle:String) = Conversation(server,author,new Date().getTime,slides,subject,tag,jid,newTitle,created,permissions,blackList)
	def replacePermissions(newPermissions:Permissions) = Conversation(server,author,new Date().getTime,slides,subject,tag,jid,title,created,newPermissions,blackList)	
	def shouldDisplayFor(username:String,userGroups:List[String]):Boolean = {
		val trimmedSubj = subject.toLowerCase.trim
		(author.toLowerCase.trim == username.toLowerCase.trim || userGroups.exists(ug => ug.toLowerCase.trim == trimmedSubj)) && trimmedSubj != "deleted"
	}
	def replaceSubject(newSubject:String) = Conversation(server,author,new Date().getTime,slides,newSubject,tag,jid,title,created,permissions,blackList)
	def addSlideAtIndex(index:Int) = {
		val oldSlides = slides.map(s => {
			if (s.index >= index){
				s.replaceIndex(s.index + 1)
			} else {
				s
			}
		})
		val newId = slides.map(s => s.id).max + 1
		val newSlides = Slide(server,author,newId,index) :: oldSlides
		replaceSlides(newSlides)
	}
	def replaceSlides(newSlides:List[Slide]) = Conversation(server,author,new Date().getTime,newSlides,subject,tag,jid,title,created,permissions,blackList)
}
object Conversation{
  def empty = Conversation(ServerConfiguration.empty,"",0L,List.empty[Slide],"","",0,"","",Permissions.default(ServerConfiguration.empty))
}

case class Slide(override val server:ServerConfiguration,author:String,id:Int,index:Int,defaultHeight:Int = 540, defaultWidth:Int = 720, exposed:Boolean = false, slideType:String = "SLIDE") extends MeTLXml(server){
	def replaceIndex(newIndex:Int) = Slide(server,author,id,newIndex,defaultHeight,defaultWidth,exposed,slideType)
}
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

case class MeTLStanza(override val server:ServerConfiguration,author:String,timestamp:Long) extends MeTLXml(server){
	def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLStanza = Stopwatch.time("MeTLStanza.adjustTimestamp", () => {
		MeTLStanza(server,author,newTime)
	})
}
object MeTLStanza{
  def empty = MeTLStanza(ServerConfiguration.empty,"",0L)
}

case class MeTLCanvasContent(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,target:String,privacy:Privacy,slide:String,identity:String,scaleFactorX:Double = 1.0,scaleFactorY:Double = 1.0) extends MeTLStanza(server,author,timestamp) {
	protected def genNewIdentity(role:String) = "%s:%s:%s_from:%s".format(new java.util.Date().getTime.toString,author,role,identity).reverse.take(256).reverse
	def left:Double = 0.0
	def right:Double = 0.0
	def top:Double = 0.0
	def bottom:Double = 0.0
	def scale(factor:Double):MeTLCanvasContent = MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,factor,factor)
	def scale(xScale:Double,yScale:Double):MeTLCanvasContent = MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,xScale,yScale)
	def alterPrivacy(newPrivacy:Privacy):MeTLCanvasContent = MeTLCanvasContent(server,author,timestamp,target,newPrivacy,slide,identity,scaleFactorX,scaleFactorY)
	def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double) = MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
	override def adjustTimestamp(newTimestamp:Long) = MeTLCanvasContent(server,author,newTimestamp,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
	def generateDirty(dirtyTime:Long) = MeTLCanvasContent.empty
	def matches(other:MeTLCanvasContent):Boolean = other.identity == identity && other.privacy == privacy && other.slide == slide
	def isDirtiedBy(other:MeTLCanvasContent):Boolean = false
	def isDirtierFor(other:MeTLCanvasContent):Boolean = false
	def generateNewIdentity(descriptor:String):MeTLCanvasContent = MeTLCanvasContent(server,author,timestamp,target,privacy,slide,genNewIdentity("newCanvasContent:"+descriptor),scaleFactorX,scaleFactorY)
}
object MeTLCanvasContent{
	def empty = MeTLCanvasContent(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","")
}

case class MeTLInk(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,checksum:Double,startingSum:Double,points:List[Point],color:Color,thickness:Double,isHighlighter:Boolean,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val scaleFactorX:Double = 1.0,override val scaleFactorY:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,scaleFactorX,scaleFactorY) {
  private def offsetAt(point:Point) = point.thickness/256*thickness
	override def matches(other:MeTLCanvasContent) = other match {
		case o:MeTLInk => super.matches(o)
		case _ => false
	}
	override def isDirtiedBy(other:MeTLCanvasContent) = other match {
		case o:MeTLDirtyInk => matches(o) && o.timestamp > timestamp 
		case _ => false
	}
  override val left:Double = points.map(p => p.x-offsetAt(p)).min
  override val right:Double = points.map(p => p.x+offsetAt(p)).max
  override val top:Double = points.map(p => p.y-offsetAt(p)).min
  override val bottom:Double = points.map(p => p.y+offsetAt(p)).max
	override def scale(factor:Double):MeTLInk = scale(factor,factor)
  override def scale(xScale:Double,yScale:Double):MeTLInk = Stopwatch.time("MeTLInk.scale", () => {
		val averageFactor = (xScale + yScale) / 2
    MeTLInk(server,author,timestamp,checksum,startingSum,points.map(p => Point(p.x*xScale,p.y*yScale,p.thickness)),color,thickness*averageFactor,isHighlighter,target,privacy,slide,identity,scaleFactorX * xScale,scaleFactorY * yScale)
  })
	override def alterPrivacy(newPrivacy:Privacy):MeTLInk = Stopwatch.time("MeTLInk.alterPrivacy", () => {
		newPrivacy match {
			case p:Privacy if (p == privacy) => this
			case Privacy.NOT_SET => this
			case p:Privacy => MeTLInk(server,author,timestamp,checksum,startingSum,points,color,thickness,isHighlighter,target,p,slide,identity,scaleFactorX,scaleFactorY)
			case _ => this
		}
	})
	override def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double) = Stopwatch.time("MeTLInk.adjustVisual", () => {
			val averageFactor = (xScale + yScale) / 2
			val newPoints = (xTranslate,yTranslate,xScale,yScale) match {
				case (0,0,1.0,1.0) => points
				case (xO,yO,1.0,1.0) => points.map(p => Point(p.x+xO,p.y+yO,p.thickness))
				case (0,0,xS,yS) => points.map(p => Point((((p.x - left) * xS) + left),(((p.y - top) * yS) + top),p.thickness)) 
				case (xO,yO,xS,yS) => points.map(p => Point((((p.x - left) * xS) + left + xO),(((p.y - top) * yS) + top + yO),p.thickness))
			}
			MeTLInk(server,author,timestamp,checksum,startingSum,newPoints,color,thickness * averageFactor,isHighlighter,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
	})
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLInk = Stopwatch.time("MeTLInk.adjustTimestamp", () => {
			MeTLInk(server,author,newTime,checksum,startingSum,points,color,thickness,isHighlighter,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
	})
	override def generateDirty(dirtyTime:Long = new java.util.Date().getTime):MeTLDirtyInk = Stopwatch.time("MeTLInk.generateDirty", () => {
			MeTLDirtyInk(server,author,dirtyTime,target,privacy,slide,identity)
	})
	override def generateNewIdentity(descriptor:String):MeTLInk = MeTLInk(server,author,timestamp,checksum,startingSum,points,color,thickness,isHighlighter,target,privacy,slide,genNewIdentity("newInk:%s".format(descriptor)),scaleFactorX,scaleFactorY)
}

object MeTLInk{
  def empty = MeTLInk(ServerConfiguration.empty,"",0L,0.0,0.0,List.empty[Point],Color.default,0.0,false,"",Privacy.NOT_SET,"","")
}

case class MeTLImage(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,tag:String,source:Box[String],imageBytes:Box[Array[Byte]],pngBytes:Box[Array[Byte]],width:Double,height:Double,x:Double,y:Double,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val scaleFactorX:Double = 1.0,override val scaleFactorY:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,scaleFactorX,scaleFactorY) {
	override def matches(other:MeTLCanvasContent) = other match {
		case o:MeTLImage => super.matches(o)
		case _ => false
	}
	override def isDirtiedBy(other:MeTLCanvasContent) = other match {
		case o:MeTLDirtyImage => matches(o) && o.timestamp > timestamp 
		case _ => false
	}
  override val left:Double = x
  override val right:Double = x+width
  override val top:Double = y
  override val bottom:Double = y+height
	override def scale(factor:Double):MeTLImage = scale(factor,factor)
  override def scale(xScale:Double,yScale:Double):MeTLImage = Stopwatch.time("MeTLImage.scale", () => {
    MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width*xScale,height*yScale,x*xScale,y*yScale,target,privacy,slide,identity,scaleFactorX * xScale,scaleFactorY * yScale)
  })
	override def alterPrivacy(possiblyNewPrivacy:Privacy):MeTLImage = Stopwatch.time("MeTLImage.alterPrivacy", () => {
		possiblyNewPrivacy match {
			case p:Privacy if (p == privacy) => this
			case Privacy.NOT_SET => this
			case p:Privacy => MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,target,p,slide,identity,scaleFactorX,scaleFactorY)
			case _ => this
		}
	})
	override def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double):MeTLImage = Stopwatch.time("MeTLImage.adjustVisual", () => {
		MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width * xScale,height * yScale,x + xTranslate,y + yTranslate,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
	})
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLImage = Stopwatch.time("MeTLimage.adjustTimestamp", () => {
		MeTLImage(server,author,newTime,tag,source,imageBytes,pngBytes,width,height,x,y,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
	})
	override def generateDirty(dirtyTime:Long = new java.util.Date().getTime):MeTLDirtyImage = Stopwatch.time("MeTLImage.generateDirty", () => {
		MeTLDirtyImage(server,author,dirtyTime,target,privacy,slide,identity)
	})
	override def generateNewIdentity(descriptor:String):MeTLImage = MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,target,privacy,slide,genNewIdentity("newImage:%s".format(descriptor)),scaleFactorX,scaleFactorY)
}

object MeTLImage{
  def empty = MeTLImage(ServerConfiguration.empty,"",0L,"",Empty,Empty,Empty,0.0,0.0,0.0,0.0,"",Privacy.NOT_SET,"","")
}

case class MeTLText(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,text:String,height:Double,width:Double,caret:Int,x:Double,y:Double,tag:String,style:String,family:String,weight:String,size:Double,decoration:String,override val identity:String,override val target:String,override val privacy:Privacy,override val slide:String,color:Color,override val scaleFactorX:Double = 1.0,override val scaleFactorY:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,scaleFactorX,scaleFactorY) {
	override def matches(other:MeTLCanvasContent) = other match {
		case o:MeTLText => super.matches(o)
		case _ => false
	}
	override def isDirtiedBy(other:MeTLCanvasContent) = other match {
		case o:MeTLDirtyInk => matches(o) && o.timestamp > timestamp 
		case _ => false
	}
  override def left:Double = x
  override def right:Double = x+width
  override def top:Double = y
  override def bottom:Double = y+height
	override def scale(factor:Double):MeTLText = scale(factor,factor)
  override def scale(xScale:Double,yScale:Double):MeTLText = Stopwatch.time("MeTLText.scale", () => {
		val averageFactor = (xScale + yScale) / 2
    MeTLText(server,author,timestamp,text,height*yScale,width*xScale,caret,x*xScale,y*yScale,tag,style,family,weight,size*averageFactor,decoration,identity,target,privacy,slide,color,scaleFactorX * xScale,scaleFactorY * yScale)
  })
	override def alterPrivacy(possiblyNewPrivacy:Privacy):MeTLText = Stopwatch.time("MeTLText.alterPrivacy", () => {
		possiblyNewPrivacy match {
			case p:Privacy if (p == privacy) => this
			case Privacy.NOT_SET => this
			case p:Privacy => MeTLText(server,author,timestamp,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,identity,target,p,slide,color,scaleFactorX,scaleFactorY)
			case _ => this
		}
	})
	override def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double):MeTLText = Stopwatch.time("MeTLText.adjustVisual", () => {
		val averageFactor = (xScale + yScale) / 2
		MeTLText(server,author,timestamp,text,height * yScale,width * xScale,caret,x + xTranslate,y + yTranslate,tag,style,family,weight,size * averageFactor,decoration,identity,target,privacy,slide,color,scaleFactorX,scaleFactorY)
	})
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLText = Stopwatch.time("MeTLText.adjustTimestamp", () => {
		MeTLText(server,author,newTime,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,identity,target,privacy,slide,color,scaleFactorX,scaleFactorY)	
	})
	override def generateDirty(dirtyTime:Long = new java.util.Date().getTime):MeTLDirtyText = Stopwatch.time("MeTLText.generateDirty", () => {
		MeTLDirtyText(server,author,dirtyTime,target,privacy,slide,identity)
	})
	override def generateNewIdentity(descriptor:String):MeTLText = MeTLText(server,author,timestamp,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,genNewIdentity("newText:%s".format(descriptor)),target,privacy,slide,color,scaleFactorX,scaleFactorY)	
}

object MeTLText{
  def empty = MeTLText(ServerConfiguration.empty,"",0L,"",0.0,0.0,0,0.0,0.0,"","","","",0.0,"","","",Privacy.NOT_SET,"",Color.default)
}

case class MeTLMoveDelta(override val server:ServerConfiguration, override val author:String,override val timestamp:Long,override val target:String, override val privacy:Privacy,override val slide:String,override val identity:String,xOrigin:Double,yOrigin:Double,inkIds:Seq[String],textIds:Seq[String],imageIds:Seq[String],xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double,newPrivacy:Privacy,isDeleted:Boolean) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity){
	override def matches(other:MeTLCanvasContent) = other match {
		case o:MeTLMoveDelta => super.matches(o)
		case _ => false
	}
	override def generateNewIdentity(descriptor:String):MeTLMoveDelta = MeTLMoveDelta(server,author,timestamp,target,privacy,slide,genNewIdentity("newMeTLMoveDelta:"+descriptor),xOrigin,yOrigin,inkIds,textIds,imageIds,xTranslate,yTranslate,xScale,yScale,newPrivacy,isDeleted)
	def generateDirtier(newInkIds:Seq[String],newTextIds:Seq[String],newImageIds:Seq[String],replacementPrivacy:Privacy):MeTLMoveDelta = Stopwatch.time("MeTLMoveDelta.generateDirtier", () => {
		MeTLMoveDelta(server,author,timestamp,target,replacementPrivacy,slide,genNewIdentity("dirtierGeneratedFrom(%s)".format(identity)),xOrigin,yOrigin,newInkIds,newTextIds,newImageIds,0.0,0.0,1.0,1.0,Privacy.NOT_SET,true)
	})
	def replaceIds(newInkIds:Seq[String],newTextIds:Seq[String],newImageIds:Seq[String],replacementPrivacy:Privacy):MeTLMoveDelta = Stopwatch.time("MeTLMoveDelta.replaceIds", () => {
		MeTLMoveDelta(server,author,timestamp,target,replacementPrivacy,slide,genNewIdentity("adjusterGeneratedFrom(%s)".format(identity)),xOrigin,yOrigin,newInkIds,newTextIds,newImageIds,xTranslate,yTranslate,xScale,yScale,Privacy.NOT_SET,isDeleted)
	})
	override def adjustVisual(newXTranslate:Double,newYTranslate:Double,newXScale:Double,newYScale:Double):MeTLMoveDelta = Stopwatch.time("MeTLMoveDelta.adjustVisual", () => {
		MeTLMoveDelta(server,author,timestamp,target,privacy,slide,identity,xOrigin + newXTranslate,yOrigin + newYTranslate,inkIds,textIds,imageIds,xTranslate + newXTranslate,yTranslate + newYTranslate,xScale * newXScale,yScale * newYScale,newPrivacy,isDeleted)
	})
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLMoveDelta = Stopwatch.time("MeTLMoveDelta.adjustTimestamp", () => {
		MeTLMoveDelta(server,author,newTime,target,privacy,slide,identity,xOrigin,yOrigin,inkIds,textIds,imageIds,xTranslate,yTranslate,xScale,yScale,newPrivacy,isDeleted)
	})
	override def scale(newXScale:Double,newYScale:Double):MeTLMoveDelta = {
		MeTLMoveDelta(server,author,timestamp,target,privacy,slide,identity,xOrigin,yOrigin,inkIds,textIds,imageIds,xTranslate * newXScale,yTranslate * newYScale,xScale * newXScale,yScale * newYScale,newPrivacy,isDeleted)
	}
	def adjustIndividualContent(cc:MeTLCanvasContent,shouldTestPrivacy:Boolean = true,possiblyOverrideLeftBounds:Double = 0.0,possiblyOverrideTopBounds:Double = 0.0):MeTLCanvasContent = {
		val thisMdLeft = xOrigin match {
			case Double.NaN => possiblyOverrideLeftBounds
			case d:Double => d
			case _ => possiblyOverrideLeftBounds
		}	
		val thisMdTop = yOrigin match {
			case Double.NaN => possiblyOverrideTopBounds
			case d:Double => d
			case _ => possiblyOverrideTopBounds
		}
		val internalX = cc.left - thisMdLeft
		val internalY = cc.top - thisMdTop
		val offsetX = -(internalX - (internalX * xScale));
		val offsetY = -(internalY - (internalY * yScale));
		cc match {
			case i:MeTLInk if (isDirtierFor(i,shouldTestPrivacy)) => i.adjustVisual(xTranslate + offsetX,yTranslate + offsetY,xScale,yScale).adjustTimestamp(timestamp).alterPrivacy(newPrivacy)
			case t:MeTLText if (isDirtierFor(t,shouldTestPrivacy)) => t.adjustVisual(xTranslate + offsetX,yTranslate + offsetY,xScale,yScale).adjustTimestamp(timestamp).alterPrivacy(newPrivacy)
			case i:MeTLImage if (isDirtierFor(i,shouldTestPrivacy)) => i.adjustVisual(xTranslate + offsetX,yTranslate + offsetY,xScale,yScale).adjustTimestamp(timestamp).alterPrivacy(newPrivacy)
			case _ => cc
		}
	}
	def generateChanges(rawPublicHistory:History,rawPrivateHistory:History):Tuple2[List[MeTLStanza],List[MeTLStanza]] = Stopwatch.time("MeTLMoveDelta.generateChanges", () => {
		val privateHistory = rawPrivateHistory.filterCanvasContentsForMoveDelta(this)
		val publicHistory = rawPublicHistory.filterCanvasContentsForMoveDelta(this)
		val (publicTexts,publicHighlighters,publicInks,publicImages) = publicHistory.getRenderableGrouped
		val (privateTexts,privateHighlighters,privateInks,privateImages) = privateHistory.getRenderableGrouped
		newPrivacy match {				
			case p:Privacy if p == Privacy.PUBLIC => {
				val notP = Privacy.PRIVATE
				val privateInksToPublicize = privateInks.map(i => adjustIndividualContent(i,false).generateNewIdentity("adjustedBy(%s)".format(identity)))
				val privateHighlightersToPublicize = privateHighlighters.map(i => adjustIndividualContent(i,false).generateNewIdentity("adjustedBy(%s)".format(identity)))
				val privateTextsToPublicize = privateTexts.map(i => adjustIndividualContent(i,false).generateNewIdentity("adjustedBy(%s)".format(identity))) 
				val privateImagesToPublicize = privateImages.map(i => adjustIndividualContent(i,false).generateNewIdentity("adjustedBy(%s)".format(identity)))
				val privateDirtier = ((privateInks ::: privateHighlighters ::: privateTexts ::: privateImages).length > 0) match {
					case true => List(generateDirtier(privateInks.map(i => i.identity) ::: privateHighlighters.map(i => i.identity),privateTexts.map(i => i.identity),privateImages.map(i => i.identity),notP))
					case _ => List.empty[MeTLStanza]
				}
				val publicAdjuster = ((publicInks ::: publicHighlighters ::: publicTexts ::: publicImages).length > 0) match {
					case true => List(replaceIds(publicInks.map(i=>i.identity) ::: publicHighlighters.map(i => i.identity),publicTexts.map(i=>i.identity),publicImages.map(i=>i.identity),p))
					case _ => List.empty[MeTLStanza]
				}
				(publicAdjuster ::: privateInksToPublicize ::: privateHighlightersToPublicize ::: privateTextsToPublicize ::: privateImagesToPublicize, privateDirtier)
			}
			case p:Privacy if p == Privacy.PRIVATE => {
				val notP = Privacy.PUBLIC
				val publicInksToPrivatize = publicInks.map(i => adjustIndividualContent(i,false).generateNewIdentity("adjustedBy(%s)".format(identity)))
				val publicHighlightersToPrivatize = publicHighlighters.map(i => adjustIndividualContent(i,false).generateNewIdentity("adjustedBy(%s)".format(identity)))
				val publicTextsToPrivatize = publicTexts.map(i => adjustIndividualContent(i,false).generateNewIdentity("adjustedBy(%s)".format(identity)))
				val publicImagesToPrivatize = publicImages.map(i => adjustIndividualContent(i,false).generateNewIdentity("adjustedBy(%s)".format(identity)))
				val publicDirtiers = ((publicInks ::: publicHighlighters ::: publicTexts ::: publicImages).length > 0) match {
					case true => List(generateDirtier(publicInks.map(i => i.identity) ::: publicHighlighters.map(i => i.identity),publicTexts.map(i => i.identity),publicImages.map(i => i.identity),notP))
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
	override def isDirtierFor(other:MeTLCanvasContent):Boolean = isDirtierFor(other,true)
	def isDirtierFor(other:MeTLCanvasContent, testPrivacy:Boolean = true):Boolean = other match {
		case i:MeTLInk => ((!testPrivacy) || privacy == i.privacy) && timestamp > i.timestamp && i.slide == slide && inkIds.contains(i.identity) 
		case i:MeTLImage => ((!testPrivacy) || privacy == i.privacy) && timestamp > i.timestamp && i.slide == slide && imageIds.contains(i.identity) 
		case i:MeTLText => ((!testPrivacy) || privacy == i.privacy) && timestamp > i.timestamp && i.slide == slide && textIds.contains(i.identity) 
		case _ => false
	}
}
case object MeTLMoveDelta{ 
  def empty = MeTLMoveDelta(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","",0.0,0.0,Nil,Nil,Nil,0.0,0.0,1.0,1.0,Privacy.NOT_SET,false)
}

case class MeTLDirtyInk(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity) {
	override def matches(other:MeTLCanvasContent) = other match {
		case o:MeTLDirtyInk => super.matches(o)
		case _ => false
	}
	override def isDirtierFor(other:MeTLCanvasContent) = other match {
		case o:MeTLInk => super.matches(o) && o.timestamp < timestamp 
		case _ => false
	}
	override def alterPrivacy(newPrivacy:Privacy):MeTLDirtyInk = MeTLDirtyInk(server,author,timestamp,target,newPrivacy,slide,identity)
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLDirtyInk = Stopwatch.time("MeTLDirtyInk.adjustTimestamp", () => {
		MeTLDirtyInk(server,author,newTime,target,privacy,slide,identity)
	})
	override def generateNewIdentity(descriptor:String):MeTLDirtyInk = MeTLDirtyInk(server,author,timestamp,target,privacy,slide,genNewIdentity("newMeTLDirtyInk:"+descriptor))
}
object MeTLDirtyInk{
  def empty = MeTLDirtyInk(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","")
}

case class MeTLDirtyText(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity){
	override def matches(other:MeTLCanvasContent) = other match {
		case o:MeTLDirtyText => super.matches(o)
		case _ => false
	}
	override def isDirtierFor(other:MeTLCanvasContent) = other match {
		case o:MeTLText => super.matches(o) && o.timestamp < timestamp 
		case _ => false
	}
	override def alterPrivacy(newPrivacy:Privacy):MeTLDirtyText = MeTLDirtyText(server,author,timestamp,target,newPrivacy,slide,identity)
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLDirtyText = Stopwatch.time("MeTLDirtyText.adjustTimestamp", () => {
		MeTLDirtyText(server,author,newTime,target,privacy,slide,identity)
	})
	override def generateNewIdentity(descriptor:String):MeTLDirtyText = MeTLDirtyText(server,author,timestamp,target,privacy,slide,genNewIdentity("newMeTLDirtyText:"+descriptor))
}
object MeTLDirtyText{
  def empty = MeTLDirtyText(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","")
}

case class MeTLDirtyImage(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity) {
	override def matches(other:MeTLCanvasContent) = other match {
		case o:MeTLDirtyImage => super.matches(o)
		case _ => false
	}
	override def isDirtierFor(other:MeTLCanvasContent) = other match {
		case o:MeTLImage => super.matches(o) && o.timestamp < timestamp 
		case _ => false
	}
	override def alterPrivacy(newPrivacy:Privacy):MeTLDirtyImage = MeTLDirtyImage(server,author,timestamp,target,newPrivacy,slide,identity)
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLDirtyImage = Stopwatch.time("MeTLDirtyImage.adjustTimestamp", () => {
		MeTLDirtyImage(server,author,newTime,target,privacy,slide,identity)
	})
	override def generateNewIdentity(descriptor:String):MeTLDirtyImage = MeTLDirtyImage(server,author,timestamp,target,privacy,slide,genNewIdentity("newMeTLDirtyImage:"+descriptor))
}
object MeTLDirtyImage{
  def empty = MeTLDirtyImage(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","")
}

case class MeTLCommand(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,command:String,commandParameters:List[String]) extends MeTLStanza(server,author,timestamp){
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLCommand = Stopwatch.time("MeTLCommand.adjustTimestamp", () => {
		MeTLCommand(server,author,newTime,command,commandParameters)	
	})
}
object MeTLCommand{
  def empty = MeTLCommand(ServerConfiguration.empty,"",0L,"/No_Command",List.empty[String])
}

case class MeTLQuiz(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,created:Long,question:String,id:String,url:Box[String],imageBytes:Box[Array[Byte]],isDeleted:Boolean,options:List[QuizOption]) extends MeTLStanza(server,author,timestamp){
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLQuiz = Stopwatch.time("MeTLQuiz.adjustTimestamp", () => {
		MeTLQuiz(server,author,newTime,created,question,id,url,imageBytes,isDeleted,options)
	})
	def replaceQuestion(newQ:String) = MeTLQuiz(server,author,timestamp,created,newQ,id,url,imageBytes,isDeleted,options)
	def addOption(newO:QuizOption) = MeTLQuiz(server,author,timestamp,created,question,id,url,imageBytes,isDeleted,options ::: List(newO.adjustName(QuizOption.nextName(options))))
	def replaceImage(newImageUrl:Box[String]) = MeTLQuiz(server,author,timestamp,created,question,id,newImageUrl,Empty,isDeleted,options)
	def replaceOption(optionName:String,newText:String) = options.find(o => o.name == optionName).map(or => MeTLQuiz(server,author,timestamp,created,question,id,url,imageBytes,isDeleted,options.filterNot(o => o == or) ::: List(or.adjustText(newText)))).getOrElse(MeTLQuiz(server,author,timestamp,created,question,id,url,imageBytes,isDeleted,options))
	def removeOption(optionName:String) = options.find(o => o.name == optionName).map(or => MeTLQuiz(server,author,timestamp,created,question,id,url,imageBytes,isDeleted,options.filterNot(o => o == or).foldLeft(List.empty[QuizOption])((os,o)=> o.adjustName(QuizOption.nextName(os)) :: os))).getOrElse(MeTLQuiz(server,author,timestamp,created,question,id,url,imageBytes,isDeleted,options))
	def delete = MeTLQuiz(server,author,timestamp,created,question,id,url,imageBytes,true,options)
}
object MeTLQuiz{
  def empty = MeTLQuiz(ServerConfiguration.empty,"",0L,0L,"","",Empty,Empty,true,List.empty[QuizOption])
}

case class MeTLSubmission(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,title:String,slideJid:Int,url:String,imageBytes:Box[Array[Byte]] = Empty,blacklist:List[SubmissionBlacklistedPerson] = List.empty[SubmissionBlacklistedPerson], override val target:String = "submission",override val privacy:Privacy = Privacy.PUBLIC,override val identity:String = new Date().getTime.toString) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slideJid.toString,identity){
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLSubmission = Stopwatch.time("MeTLSubmission.adjustTimestamp", () => {
		MeTLSubmission(server,author,newTime,title,slideJid,url,imageBytes,blacklist,target,privacy,identity)
	})
}
object MeTLSubmission{
  def empty = MeTLSubmission(ServerConfiguration.empty,"",0L,"",0,"")
}
case class SubmissionBlacklistedPerson(username:String,highlight:Color)
object SubmissionBlacklistedPerson{
	def empty = SubmissionBlacklistedPerson("",Color.default)
}

case class QuizOption(name:String,text:String,correct:Boolean = false,color:Color = Color.default){
	def adjustName(newName:String) = QuizOption(newName,text,correct,QuizOption.colorForName(newName))
	def adjustText(newText:String) = QuizOption(name,newText,correct,color)
}
object QuizOption{
  def empty = QuizOption("","",false,Color.default)
	def colorForName(optionName:String):Color = optionName.toLowerCase.toArray[Char].reverse.headOption.map(ho => ((ho - 'a').asInstanceOf[Int] % 2) match {
		case 0 => Color(255,255,255,255)
		case 1 => Color(255,70,130,180)
	}).getOrElse(Color(255,255,255,255))
	def numberToName(number:Int):String = {
		if (number > 0){
			val numberPart = number % 26 match {
				case 0 => 26
				case n => n
			}
			val numberRest = number / 26
			val thisChar = (numberPart + 'a' - 1).asInstanceOf[Char] 
			if (((number - 1) / 26) > 0)
				(numberToName(numberRest).toArray[Char].toList ::: List(thisChar)).toArray.mkString.toUpperCase 
			else 
				thisChar.toString.toUpperCase
		} else ""
	}
	def nameToNumber(name:String):Int = {
		name.toLowerCase.toArray[Char].foldLeft(0)((a,i) => (a * 26) + i - 'a' + 1)
	}
	def nextName(options:List[QuizOption] = List.empty[QuizOption]) = {
		options match {
			case l:List[QuizOption] if (l.length > 0) => numberToName(options.map(o => nameToNumber(o.name)).distinct.max + 1)
			case _ => "A"
		}
	}
}

case class MeTLQuizResponse(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,answer:String,answerer:String,id:String) extends MeTLStanza(server,author,timestamp){
	override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLQuizResponse = Stopwatch.time("MeTLQuizResponse.adjustTimestamp", () => {
		MeTLQuizResponse(server,author,newTime,answer,answerer,id)
	})
}
object MeTLQuizResponse{
  def empty = MeTLQuizResponse(ServerConfiguration.empty,"",0L,"","","")
}
