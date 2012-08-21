package com.metl.model

import net.liftweb.common._
import net.liftweb.util.Helpers._

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
	private	def convert2AfterN(h:String,n:Int):Int = hexToInt(h.drop(n).take(2).mkString)
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

case class Conversation(override val server:ServerConfiguration,author:String,lastAccessed:Long,slides:List[Slide],subject:String,tag:String,jid:Int,title:String,created:String,permissions:Permissions) extends MeTLXml(server)
object Conversation{
	def empty = Conversation(ServerConfiguration.empty,"",0L,List.empty[Slide],"","",0,"","",Permissions.default(ServerConfiguration.empty))
}

case class Slide(override val server:ServerConfiguration,author:String,id:Int,index:Int) extends MeTLXml(server)
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

case class MeTLCanvasContent(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,target:String,privacy:Privacy,slide:String,identity:String,scaleFactor:Double = 1.0) extends MeTLStanza(server,author,timestamp) 

case class MeTLInk(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,checksum:Double,startingSum:Double,points:List[Point],color:Color,thickness:Double,isHighlighter:Boolean,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val scaleFactor:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,scaleFactor) {
	private def offsetAt(point:Point) = point.thickness/256*thickness
	val left:Double = points.map(p => p.x-offsetAt(p)).min
	val right:Double = points.map(p => p.x+offsetAt(p)).max
	val top:Double = points.map(p => p.y-offsetAt(p)).min
	val bottom:Double = points.map(p => p.y+offsetAt(p)).max
	def scale(factor:Double):MeTLInk = Stopwatch.time("MeTLInk.scale", () => {
		MeTLInk(server,author,timestamp,checksum,startingSum,points.map(p => Point(p.x*factor,p.y*factor,p.thickness)),color,thickness*factor,isHighlighter,target,privacy,slide,identity,factor)
	})
}

object MeTLInk{
	def empty = MeTLInk(ServerConfiguration.empty,"",0L,0.0,0.0,List.empty[Point],Color.default,0.0,false,"",Privacy.NOT_SET,"","")
}

case class MeTLImage(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,tag:String,source:Box[String],imageBytes:Box[Array[Byte]],pngBytes:Box[Array[Byte]],width:Double,height:Double,x:Double,y:Double,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val scaleFactor:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,scaleFactor) {
  val left:Double = x
  val right:Double = x+width
  val top:Double = y
  val bottom:Double = y+height
	def scale(factor:Double):MeTLImage = Stopwatch.time("MeTLImage.scale", () => {
		MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width*factor,height*factor,x*factor,y*factor,target,privacy,slide,identity,factor)
	})
}

object MeTLImage{
	def empty = MeTLImage(ServerConfiguration.empty,"",0L,"",Empty,Empty,Empty,0.0,0.0,0.0,0.0,"",Privacy.NOT_SET,"","")
}

case class MeTLText(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,text:String,height:Double,width:Double,caret:Int,x:Double,y:Double,tag:String,style:String,family:String,weight:String,size:Double,decoration:String,override val identity:String,override val target:String,override val privacy:Privacy,override val slide:String,color:Color,override val scaleFactor:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,scaleFactor) {
	def left:Double = x
	def right:Double = x+width
	def top:Double = y
	def bottom:Double = y+height
	def scale(factor:Double):MeTLText = Stopwatch.time("MeTLText.scale", () => {
		MeTLText(server,author,timestamp,text,height*factor,width*factor,caret,x*factor,y*factor,tag,style,family,weight,size*factor,decoration,identity,target,privacy,slide,color,factor)
	})
}

object MeTLText{
	def empty = MeTLText(ServerConfiguration.empty,"",0L,"",0.0,0.0,0,0.0,0.0,"","","","",0.0,"","","",Privacy.NOT_SET,"",Color.default)
}

case class MeTLDirtyInk(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity) 
object MeTLDirtyInk{
	def empty = MeTLDirtyInk(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","")
}

case class MeTLDirtyText(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity)
object MeTLDirtyText{
	def empty = MeTLDirtyText(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","")
}

case class MeTLDirtyImage(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity)
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

case class MeTLSubmission(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,slide:Int,url:String) extends MeTLStanza(server,author,timestamp)
object MeTLSubmission{
	def empty = MeTLSubmission(ServerConfiguration.empty,"",0L,0,"")
}

case class QuizOption(name:String,text:String,correct:Boolean,color:Color)
object QuizOption{
	def empty = QuizOption("","",false,Color.default)
}

case class MeTLQuizResponse(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,answer:String,answerer:String,id:String) extends MeTLStanza(server,author,timestamp)
object MeTLQuizResponse{
	def empty = MeTLQuizResponse(ServerConfiguration.empty,"",0L,"","","")
}
