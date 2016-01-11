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

case class Presentation(override val server:ServerConfiguration,conversation:Conversation,stanzas:Map[Int,List[MeTLStanza]] = Map.empty[Int,List[MeTLStanza]],metaData:List[Tuple2[String,String]] = List.empty[Tuple2[String,String]],override val audiences:List[Audience] = Nil) extends MeTLData(server,audiences)
object Presentation{
  def emtpy = Presentation(ServerConfiguration.empty,Conversation.empty)
}

case class GroupSet(override val server:ServerConfiguration,id:String,location:String,groupingStrategy:GroupingStrategy,groups:List[Group],override val audiences:List[Audience] = Nil) extends MeTLData(server,audiences)
object GroupSet {
  def empty = GroupSet(ServerConfiguration.empty,"","",EveryoneInOneGroup,Nil,Nil)
}

abstract class GroupingStrategy{
  def addNewPerson(g:GroupSet,person:String):GroupSet
}

case class ByMaximumSize(groupSize:Int) extends GroupingStrategy {
  override def addNewPerson(g:GroupSet,person:String):GroupSet = {
    val oldGroups = g.groups
    val newGroups = {
      g.groups.find(group => {
        group.members.length < groupSize
      }).map(fg => {
        fg.copy(members = person :: fg.members) :: oldGroups.filter(_.id != fg.id)
      }).getOrElse({
        Group(g.server,nextFuncName,g.location,List(person)) :: oldGroups
      })
    }
    g.copy(groups = newGroups)
  }
}
case class ByTotalGroups(numberOfGroups:Int) extends GroupingStrategy {
  override def addNewPerson(g:GroupSet,person:String):GroupSet = {
    val oldGroups = g.groups
    g.copy(groups = {
      oldGroups match {
        case l:List[Group] if l.length < numberOfGroups => Group(g.server,nextFuncName,g.location,List(person)) :: l
        case l:List[Group] => l.sortWith((a,b) => a.members.length < b.members.length).headOption.map(fg => {
          fg.copy(members = person :: fg.members) :: l.filter(_.id != fg.id)
        }).getOrElse({
          l.head.copy(members = person :: l.head.members) :: l.drop(1)
        })
      }
    })
  }
}
case class ComplexGroupingStrategy(data:Map[String,String]) extends GroupingStrategy {
  protected var groupingFunction:Tuple2[GroupSet,String]=>GroupSet = (t:Tuple2[GroupSet,String]) => t._1
  override def addNewPerson(g:GroupSet,person:String):GroupSet = {
    groupingFunction((g,person))
  }
  def replaceGroupingFunction(func:Tuple2[GroupSet,String]=>GroupSet):ComplexGroupingStrategy = {
    groupingFunction = func
    this
  }
}
case object OnePersonPerGroup extends GroupingStrategy {
  override def addNewPerson(g:GroupSet,person:String):GroupSet = {
    g.copy(groups = Group(g.server,nextFuncName,g.location,List(person)) :: g.groups)
  }
}
case object EveryoneInOneGroup extends GroupingStrategy {
  override def addNewPerson(g:GroupSet,person:String):GroupSet = {
    g.copy(groups = List(Group(g.server,g.groups.headOption.map(_.id).getOrElse(nextFuncName),g.location,(person :: g.groups.flatMap(_.members)).distinct)))
  }
}

case class Group(override val server:ServerConfiguration,id:String,location:String,members:List[String],override val audiences:List[Audience] = Nil) extends MeTLData(server,audiences)
object Group {
  def empty = Group(ServerConfiguration.empty,"","",Nil,Nil)
}

case class Conversation(override val server:ServerConfiguration,author:String,lastAccessed:Long,slides:List[Slide],subject:String,tag:String,jid:Int,title:String,created:String,permissions:Permissions, blackList:List[String] = List.empty[String],override val audiences:List[Audience] = Nil) extends MeTLData(server,audiences){
  def delete = copy(subject="deleted",lastAccessed=new Date().getTime)//Conversation(server,author,new Date().getTime,slides,"deleted",tag,jid,title,created,permissions,blackList,audiences)
  def rename(newTitle:String) = copy(title=newTitle,lastAccessed = new Date().getTime)//Conversation(server,author,new Date().getTime,slides,subject,tag,jid,newTitle,created,permissions,blackList,audiences)
  def replacePermissions(newPermissions:Permissions) = copy(permissions = newPermissions, lastAccessed = new Date().getTime)//Conversation(server,author,new Date().getTime,slides,subject,tag,jid,title,created,newPermissions,blackList,audiences)
  def shouldDisplayFor(username:String,userGroups:List[String]):Boolean = {
    val trimmedSubj = subject.toLowerCase.trim
    (trimmedSubj == "unrestricted" || author.toLowerCase.trim == username.toLowerCase.trim || userGroups.exists(ug => ug.toLowerCase.trim == trimmedSubj)) && trimmedSubj != "deleted"
  }
  def replaceSubject(newSubject:String) = copy(subject=newSubject,lastAccessed=new Date().getTime)//Conversation(server,author,new Date().getTime,slides,newSubject,tag,jid,title,created,permissions,blackList)
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
  def replaceSlides(newSlides:List[Slide]) = copy(slides=newSlides,lastAccessed = new Date().getTime)//Conversation(server,author,new Date().getTime,newSlides,subject,tag,jid,title,created,permissions,blackList)
}
object Conversation{
  def empty = Conversation(ServerConfiguration.empty,"",0L,List.empty[Slide],"","",0,"","",Permissions.default(ServerConfiguration.empty),Nil,Nil)
}

case class Slide(override val server:ServerConfiguration,author:String,id:Int,index:Int,defaultHeight:Int = 540, defaultWidth:Int = 720, exposed:Boolean = false, slideType:String = "SLIDE",groupSet:List[GroupSet] = Nil,override val audiences:List[Audience] = Nil) extends MeTLData(server,audiences){
  def replaceIndex(newIndex:Int) = copy(index=newIndex)//Slide(server,author,id,newIndex,defaultHeight,defaultWidth,exposed,slideType)
}
object Slide{
  def empty = Slide(ServerConfiguration.empty,"",0,0)
}

case class Audience(override val server:ServerConfiguration,domain:String,name:String,audienceType:String,action:String,override val audiences:List[Audience] = Nil) extends MeTLData(server,audiences)
object Audience {
  def empty = Audience(ServerConfiguration.empty,"","","","")
  def default(server:ServerConfiguration = ServerConfiguration.default) = Audience(server,"","","","")
}

case class Permissions(override val server:ServerConfiguration, studentsCanOpenFriends:Boolean,studentsCanPublish:Boolean,usersAreCompulsorilySynced:Boolean) extends MeTLData(server,Nil)
object Permissions{
  def empty = Permissions(ServerConfiguration.empty,false,false,false)
  def default(server:ServerConfiguration = ServerConfiguration.default) = Permissions(server,false,true,false)
}

class MeTLData(val server:ServerConfiguration,val audiences:List[Audience] = Nil){
  override def equals(a:Any) = a match {
    case MeTLData(aServer,aAudiences) => aServer == server && aAudiences == audiences
    case _ => false
  }
}
object MeTLData {
  def apply(server:ServerConfiguration,audiences:List[Audience] = Nil) = new MeTLData(server,audiences)
  def unapply(in:MeTLData) = Some((in.server,in.audiences))
  def empty = MeTLData(ServerConfiguration.empty,Nil)
}

case class MeTLUnhandledData(override val server:ServerConfiguration,unhandled:String,valueType:String,override val audiences:List[Audience] = Nil) extends MeTLData(server,audiences)
object MeTLUnhandledData {
  def empty = MeTLUnhandledData(ServerConfiguration.empty,"","null")
  def empty(unhandled:String,valueType:String) = MeTLUnhandledData(ServerConfiguration.empty,unhandled,valueType)
}
case class MeTLUnhandledStanza(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,unhandled:String,valueType:String,override val audiences:List[Audience] = Nil) extends MeTLStanza(server,author,timestamp,audiences){
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime) = Stopwatch.time("MeTLUnhandledStanza.adjustTimestamp",copy(timestamp = newTime))
}
class MeTLStanza(override val server:ServerConfiguration,val author:String,val timestamp:Long,override val audiences:List[Audience] = Nil) extends MeTLData(server,audiences){
  def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLStanza = Stopwatch.time("MeTLStanza.adjustTimestamp",{
    //copy(time=newTime)//
    MeTLStanza(server,author,newTime,audiences)
  })
  override def equals(a:Any) = a match {
    case MeTLStanza(aServer,aAuthor,aTimestamp,aAudiences) => aServer == server && aAuthor == author && aTimestamp == timestamp && aAudiences == audiences
    case _ => false
  }
}
object MeTLUnhandledStanza {
  def empty = MeTLUnhandledStanza(ServerConfiguration.empty,"",0L,"","null")
  def empty(unhandled:String,valueType:String) = MeTLUnhandledStanza(ServerConfiguration.empty,"",0L,unhandled,valueType)
}
object MeTLStanza{
  def apply(server:ServerConfiguration,author:String,timestamp:Long,audiences:List[Audience] = Nil) = new MeTLStanza(server,author,timestamp,audiences)
  def unapply(in:MeTLStanza) = Some((in.server,in.author,in.timestamp,in.audiences))
  def empty = MeTLStanza(ServerConfiguration.empty,"",0L)
}

case class Attendance(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,location:String,present:Boolean,override val audiences:List[Audience]) extends MeTLStanza(server,author,timestamp,audiences){
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime) = Stopwatch.time("Attendance.adjustTimestamp",copy(timestamp = newTime))
}
object Attendance{
  def empty = Attendance(ServerConfiguration.empty,"",0L,"",false,Nil)
}

case class MeTLUnhandledCanvasContent(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val audiences:List[Audience] = Nil, override val scaleFactorX:Double = 1.0,override val scaleFactorY:Double = 1.0,unhandled:String,valueType:String) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences,scaleFactorX,scaleFactorY)
object MeTLUnhandledCanvasContent {
  def empty = MeTLUnhandledCanvasContent(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","",Nil,1.0,1.0,"","null")
  def empty(unhandled:String,valueType:String) = MeTLUnhandledCanvasContent(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","",Nil,1.0,1.0,unhandled,valueType)
}
class MeTLCanvasContent(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,val target:String,val privacy:Privacy,val slide:String,val identity:String,override val audiences:List[Audience] = Nil,val scaleFactorX:Double = 1.0,val scaleFactorY:Double = 1.0) extends MeTLStanza(server,author,timestamp,audiences) {
  protected def genNewIdentity(role:String) = "%s:%s:%s_from:%s".format(new java.util.Date().getTime.toString,author,role,identity).reverse.take(256).reverse
  def left:Double = 0.0
  def right:Double = 0.0
  def top:Double = 0.0
  def bottom:Double = 0.0
  def scale(factor:Double):MeTLCanvasContent = //copy(scaleFactorX = factor,scaleFactorY = factor)//
    MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences,factor,factor)
  def scale(xScale:Double,yScale:Double):MeTLCanvasContent = //copy(scaleFactorX = xScale,scaleFactorY = yScale)//
    MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences,xScale,yScale)
  def alterPrivacy(newPrivacy:Privacy):MeTLCanvasContent = //copy(privacy=newPrivacy)//
    MeTLCanvasContent(server,author,timestamp,target,newPrivacy,slide,identity,audiences,scaleFactorX,scaleFactorY)
  def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double) = //copy()//
    MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences,scaleFactorX,scaleFactorY)
  override def adjustTimestamp(newTimestamp:Long) = MeTLCanvasContent(server,author,newTimestamp,target,privacy,slide,identity,audiences,scaleFactorX,scaleFactorY)
  def generateDirty(dirtyTime:Long) = MeTLCanvasContent.empty
  def matches(other:MeTLCanvasContent):Boolean = other.identity == identity && other.privacy == privacy && other.slide == slide
  def isDirtiedBy(other:MeTLCanvasContent):Boolean = false
  def isDirtierFor(other:MeTLCanvasContent):Boolean = false
  def generateNewIdentity(descriptor:String):MeTLCanvasContent = //copy(identity = genNewIdentity("newCanvasContent:"+descriptor))//
    MeTLCanvasContent(server,author,timestamp,target,privacy,slide,genNewIdentity("newCanvasContent:"+descriptor),audiences,scaleFactorX,scaleFactorY)
  override def equals(a:Any) = a match {
    case MeTLCanvasContent(aServer,aAuthor,aTimestamp,aTarget,aPrivacy,aSlide,aIdentity,aAudiences,aScaleFactorX,aScaleFactorY) => aServer == server && aAuthor == author && aTimestamp == timestamp && aTarget == target && aPrivacy == privacy && aSlide == slide && aIdentity == identity && aAudiences == audiences && aScaleFactorX == scaleFactorX && aScaleFactorY == scaleFactorY
    case _ => false
  }
}
object MeTLCanvasContent{
  def apply(server:ServerConfiguration,author:String,timestamp:Long,target:String,privacy:Privacy,slide:String,identity:String,audiences:List[Audience] = Nil, scaleFactorX:Double = 1.0,scaleFactorY:Double = 1.0) = new MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences,scaleFactorX,scaleFactorY)
  def unapply(in:MeTLCanvasContent) = Some((in.server,in.author,in.timestamp,in.target,in.privacy,in.slide,in.identity,in.audiences,in.scaleFactorX,in.scaleFactorY))
  def empty = MeTLCanvasContent(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","")
}

case class MeTLInk(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,checksum:Double,startingSum:Double,points:List[Point],color:Color,thickness:Double,isHighlighter:Boolean,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val audiences:List[Audience] = Nil,override val scaleFactorX:Double = 1.0,override val scaleFactorY:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences,scaleFactorX,scaleFactorY) {
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
  override def scale(xScale:Double,yScale:Double):MeTLInk = Stopwatch.time("MeTLInk.scale",{
    val averageFactor = (xScale + yScale) / 2
    //MeTLInk(server,author,timestamp,checksum,startingSum,points.map(p => Point(p.x*xScale,p.y*yScale,p.thickness)),color,thickness*averageFactor,isHighlighter,target,privacy,slide,identity,scaleFactorX * xScale,scaleFactorY * yScale)
    copy(points = points.map(p => Point(p.x*xScale,p.y*yScale,p.thickness)),thickness = thickness*averageFactor,scaleFactorX = scaleFactorX * xScale,scaleFactorY = scaleFactorY * yScale)
  })
  override def alterPrivacy(newPrivacy:Privacy):MeTLInk = Stopwatch.time("MeTLInk.alterPrivacy",{
    newPrivacy match {
      case p:Privacy if (p == privacy) => this
      case Privacy.NOT_SET => this
      case p:Privacy => copy(privacy = p)//MeTLInk(server,author,timestamp,checksum,startingSum,points,color,thickness,isHighlighter,target,p,slide,identity,scaleFactorX,scaleFactorY)
      case _ => this
    }
  })
  override def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double) = Stopwatch.time("MeTLInk.adjustVisual",{
    val averageFactor = (xScale + yScale) / 2
    val newPoints = (xTranslate,yTranslate,xScale,yScale) match {
      case (0,0,1.0,1.0) => points
      case (xO,yO,1.0,1.0) => points.map(p => Point(p.x+xO,p.y+yO,p.thickness))
      case (0,0,xS,yS) => points.map(p => Point((((p.x - left) * xS) + left),(((p.y - top) * yS) + top),p.thickness))
      case (xO,yO,xS,yS) => points.map(p => Point((((p.x - left) * xS) + left + xO),(((p.y - top) * yS) + top + yO),p.thickness))
    }
//    MeTLInk(server,author,timestamp,checksum,startingSum,newPoints,color,thickness * averageFactor,isHighlighter,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
    copy(points = newPoints, thickness = thickness * averageFactor)
  })
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLInk = Stopwatch.time("MeTLInk.adjustTimestamp",{
    //MeTLInk(server,author,newTime,checksum,startingSum,points,color,thickness,isHighlighter,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
    copy(timestamp = newTime)  
  })
  override def generateDirty(dirtyTime:Long = new java.util.Date().getTime):MeTLDirtyInk = Stopwatch.time("MeTLInk.generateDirty",{
    MeTLDirtyInk(server,author,dirtyTime,target,privacy,slide,identity,audiences)
  })
  override def generateNewIdentity(descriptor:String):MeTLInk = copy(identity = genNewIdentity("newInk:"+descriptor))//MeTLInk(server,author,timestamp,checksum,startingSum,points,color,thickness,isHighlighter,target,privacy,slide,genNewIdentity("newInk:%s".format(descriptor)),scaleFactorX,scaleFactorY)
}

object MeTLInk{
  def empty = MeTLInk(ServerConfiguration.empty,"",0L,0.0,0.0,List.empty[Point],Color.default,0.0,false,"",Privacy.NOT_SET,"","")
}

case class MeTLImage(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,tag:String,source:Box[String],imageBytes:Box[Array[Byte]],pngBytes:Box[Array[Byte]],width:Double,height:Double,x:Double,y:Double,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val audiences:List[Audience] = Nil,override val scaleFactorX:Double = 1.0,override val scaleFactorY:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences,scaleFactorX,scaleFactorY) {
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
  override def scale(xScale:Double,yScale:Double):MeTLImage = Stopwatch.time("MeTLImage.scale",{
//    MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width*xScale,height*yScale,x*xScale,y*yScale,target,privacy,slide,identity,scaleFactorX * xScale,scaleFactorY * yScale)
    copy(width = width * xScale, height = height * yScale, x = x * xScale, y = y * yScale, scaleFactorX = scaleFactorX * xScale , scaleFactorY = scaleFactorY * yScale)
  })
  override def alterPrivacy(possiblyNewPrivacy:Privacy):MeTLImage = Stopwatch.time("MeTLImage.alterPrivacy",{
    possiblyNewPrivacy match {
      case p:Privacy if (p == privacy) => this
      case Privacy.NOT_SET => this
      case p:Privacy => copy(privacy = p)//MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,target,p,slide,identity,scaleFactorX,scaleFactorY)
      case _ => this
    }
  })
  override def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double):MeTLImage = Stopwatch.time("MeTLImage.adjustVisual",{
    copy(width = width * xScale, height = height * yScale, x = x + xTranslate, y = y+ yTranslate)//MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width * xScale,height * yScale,x + xTranslate,y + yTranslate,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
  })
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLImage = Stopwatch.time("MeTLimage.adjustTimestamp",{
//    MeTLImage(server,author,newTime,tag,source,imageBytes,pngBytes,width,height,x,y,target,privacy,slide,identity,scaleFactorX,scaleFactorY)
    copy(timestamp = newTime)
  })
  override def generateDirty(dirtyTime:Long = new java.util.Date().getTime):MeTLDirtyImage = Stopwatch.time("MeTLImage.generateDirty",{
    MeTLDirtyImage(server,author,dirtyTime,target,privacy,slide,identity)
  })
  override def generateNewIdentity(descriptor:String):MeTLImage = copy(identity = genNewIdentity("newImage:"+descriptor))//MeTLImage(server,author,timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,target,privacy,slide,genNewIdentity("newImage:%s".format(descriptor)),scaleFactorX,scaleFactorY)
}

object MeTLImage{
  def empty = MeTLImage(ServerConfiguration.empty,"",0L,"",Empty,Empty,Empty,0.0,0.0,0.0,0.0,"",Privacy.NOT_SET,"","")
}

case class MeTLText(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,text:String,height:Double,width:Double,caret:Int,x:Double,y:Double,tag:String,style:String,family:String,weight:String,size:Double,decoration:String,override val identity:String,override val target:String,override val privacy:Privacy,override val slide:String,color:Color,override val audiences:List[Audience] = Nil,override val scaleFactorX:Double = 1.0,override val scaleFactorY:Double = 1.0) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences,scaleFactorX,scaleFactorY) {
  lazy val isRichText = {
    try {
      scala.xml.XML.loadString(text).namespace.trim.toLowerCase == "http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    } catch {
      case e:Exception => false
    }
  }
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
  import scala.xml._
  protected def replaceAttr(attributes:scala.xml.MetaData,attrName:String,attrTransform:String=>String):scala.xml.MetaData = {
    attributes match {
      case p:PrefixedAttribute => new PrefixedAttribute(pre=p.pre,key=p.key,value= { p.key match {
        case an:String if an == attrName => attrTransform(p.value.text)
        case other => p.value.text
      }}, next=replaceAttr(p.next,attrName,attrTransform))
      case u:UnprefixedAttribute => new UnprefixedAttribute(key=u.key,value= { u.key match {
        case an:String if an == attrName => attrTransform(u.value.text)
        case other => u.value.text
      }}, next=replaceAttr(u.next,attrName,attrTransform))
      case _ => Null
    }
  }
  protected def scaleRichText(t:String,factor:Double):String = {
    try {
      scala.xml.XML.loadString(t) match {
        case section:Elem if section.label == "Section" => {
          section.copy(attributes = replaceAttr(section.attributes,"FontSize",(a) => (a.toDouble * factor).toString),child = {
            (section \ "Paragraph").map{
              case page:Elem if page.label == "Paragraph" => {
                page.copy(attributes = replaceAttr(page.attributes,"FontSize",(a) => (a.toDouble * factor).toString),child = {
                  (page \ "Run").map{
                    case run:Elem if run.label == "Run" => {
                      run.copy(attributes = replaceAttr(run.attributes,"FontSize",(a) => (a.toDouble * factor).toString))
                    }
                    case other => other
                  }
                })
              }
              case other => other
            }
          }).toString
        }
        case _other => t
      }
    } catch {
      case e:Exception => t
    }
  }
  override def scale(factor:Double):MeTLText = scale(factor,factor)
  override def scale(xScale:Double,yScale:Double):MeTLText = Stopwatch.time("MeTLText.scale",{
    val averageFactor = (xScale + yScale) / 2
    if (isRichText){
      copy(text = scaleRichText(text,averageFactor), height = height * yScale, width = width * xScale, x = x * xScale, y = y * yScale, size = size * averageFactor, scaleFactorX = scaleFactorX * xScale, scaleFactorY = scaleFactorY * yScale)
    } else {
      //MeTLText(server,author,timestamp,text,height*yScale,width*xScale,caret,x*xScale,y*yScale,tag,style,family,weight,size*averageFactor,decoration,identity,target,privacy,slide,color,scaleFactorX * xScale,scaleFactorY * yScale)
      copy(height = height * yScale, width = width * xScale, x = x * xScale, y = y * yScale, size = size * averageFactor, scaleFactorX = scaleFactorX * xScale, scaleFactorY = scaleFactorY * yScale)
    }
  })
  override def alterPrivacy(possiblyNewPrivacy:Privacy):MeTLText = Stopwatch.time("MeTLText.alterPrivacy",{
    possiblyNewPrivacy match {
      case p:Privacy if (p == privacy) => this
      case Privacy.NOT_SET => this
      case p:Privacy => copy(privacy = p)//MeTLText(server,author,timestamp,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,identity,target,p,slide,color,scaleFactorX,scaleFactorY)
      case _ => this
    }
  })
  override def adjustVisual(xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double):MeTLText = Stopwatch.time("MeTLText.adjustVisual",{
    val averageFactor = (xScale + yScale) / 2
    if (isRichText){
      copy(text = scaleRichText(text,averageFactor), height = height * yScale, width = width * xScale, x = x + xTranslate, y = y + yTranslate, size = size * averageFactor)
    } else {
      copy(height = height * yScale, width = width * xScale, x = x + xTranslate, y = y + yTranslate, size = size * averageFactor)
    }
  })
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLText = Stopwatch.time("MeTLText.adjustTimestamp",{
      copy(timestamp = newTime)
//    MeTLText(server,author,newTime,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,identity,target,privacy,slide,color,scaleFactorX,scaleFactorY)
  })
  override def generateDirty(dirtyTime:Long = new java.util.Date().getTime):MeTLDirtyText = Stopwatch.time("MeTLText.generateDirty",{
    MeTLDirtyText(server,author,dirtyTime,target,privacy,slide,identity)
  })
  override def generateNewIdentity(descriptor:String):MeTLText = copy(identity = genNewIdentity("newText:"+descriptor))//MeTLText(server,author,timestamp,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,genNewIdentity("newText:%s".format(descriptor)),target,privacy,slide,color,scaleFactorX,scaleFactorY)
}

object MeTLText{
  def empty = MeTLText(ServerConfiguration.empty,"",0L,"",0.0,0.0,0,0.0,0.0,"","","","",0.0,"","","",Privacy.NOT_SET,"",Color.default)
}

case class MeTLMoveDelta(override val server:ServerConfiguration, override val author:String,override val timestamp:Long,override val target:String, override val privacy:Privacy,override val slide:String,override val identity:String,xOrigin:Double,yOrigin:Double,inkIds:Seq[String],textIds:Seq[String],imageIds:Seq[String],xTranslate:Double,yTranslate:Double,xScale:Double,yScale:Double,newPrivacy:Privacy,isDeleted:Boolean,override val audiences:List[Audience] = Nil) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences){
  override def matches(other:MeTLCanvasContent) = other match {
    case o:MeTLMoveDelta => super.matches(o)
    case _ => false
  }
  override def generateNewIdentity(descriptor:String):MeTLMoveDelta = copy(identity = genNewIdentity("newMeTLMoveDelta:"+descriptor))//MeTLMoveDelta(server,author,timestamp,target,privacy,slide,genNewIdentity("newMeTLMoveDelta:"+descriptor),xOrigin,yOrigin,inkIds,textIds,imageIds,xTranslate,yTranslate,xScale,yScale,newPrivacy,isDeleted)
  def generateDirtier(newInkIds:Seq[String],newTextIds:Seq[String],newImageIds:Seq[String],replacementPrivacy:Privacy):MeTLMoveDelta = Stopwatch.time("MeTLMoveDelta.generateDirtier",{
    //MeTLMoveDelta(server,author,timestamp,target,replacementPrivacy,slide,genNewIdentity("dirtierGeneratedFrom(%s)".format(identity)),xOrigin,yOrigin,newInkIds,newTextIds,newImageIds,0.0,0.0,1.0,1.0,Privacy.NOT_SET,true)
    copy(privacy = replacementPrivacy,identity = genNewIdentity("dirtierGeneratedFrom(%s)".format(identity)),inkIds = newInkIds,textIds = newTextIds,imageIds = newImageIds,xTranslate = 0.0,yTranslate = 0.0,xScale = 1.0,yScale = 1.0,isDeleted = true)
  })
  def replaceIds(newInkIds:Seq[String],newTextIds:Seq[String],newImageIds:Seq[String],replacementPrivacy:Privacy):MeTLMoveDelta = Stopwatch.time("MeTLMoveDelta.replaceIds",{
    //MeTLMoveDelta(server,author,timestamp,target,replacementPrivacy,slide,genNewIdentity("adjusterGeneratedFrom(%s)".format(identity)),xOrigin,yOrigin,newInkIds,newTextIds,newImageIds,xTranslate,yTranslate,xScale,yScale,Privacy.NOT_SET,isDeleted)
    copy(privacy = replacementPrivacy,identity = genNewIdentity("adjusterGeneratedFrom(%s)".format(identity)),inkIds = newInkIds,textIds = newTextIds,imageIds = newImageIds)
  })
  override def adjustVisual(newXTranslate:Double,newYTranslate:Double,newXScale:Double,newYScale:Double):MeTLMoveDelta = Stopwatch.time("MeTLMoveDelta.adjustVisual",{
    //MeTLMoveDelta(server,author,timestamp,target,privacy,slide,identity,xOrigin + newXTranslate,yOrigin + newYTranslate,inkIds,textIds,imageIds,xTranslate + newXTranslate,yTranslate + newYTranslate,xScale * newXScale,yScale * newYScale,newPrivacy,isDeleted)
    copy(xOrigin = xOrigin + newXTranslate,yOrigin = yOrigin + newYTranslate,xTranslate = xTranslate + newXTranslate,yTranslate = yTranslate + newYTranslate,xScale = xScale * newXScale,yScale = yScale * newYScale,privacy = newPrivacy)
  })
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLMoveDelta = Stopwatch.time("MeTLMoveDelta.adjustTimestamp",{
//    MeTLMoveDelta(server,author,newTime,target,privacy,slide,identity,xOrigin,yOrigin,inkIds,textIds,imageIds,xTranslate,yTranslate,xScale,yScale,newPrivacy,isDeleted)
    copy(timestamp = newTime)
  })
  override def scale(newXScale:Double,newYScale:Double):MeTLMoveDelta = {
    //MeTLMoveDelta(server,author,timestamp,target,privacy,slide,identity,xOrigin,yOrigin,inkIds,textIds,imageIds,xTranslate * newXScale,yTranslate * newYScale,xScale * newXScale,yScale * newYScale,newPrivacy,isDeleted)
    copy(xTranslate = xTranslate * newXScale,yTranslate= yTranslate * newYScale,xScale = xScale * newXScale,yScale = yScale * newYScale,privacy = newPrivacy)
  }
  def adjustIndividualContent(cc:MeTLCanvasContent,shouldTestPrivacy:Boolean = true,possiblyOverrideLeftBounds:Double = 0.0,possiblyOverrideTopBounds:Double = 0.0):MeTLCanvasContent = {
    val thisMdLeft = xOrigin match {
      case Double.NaN => possiblyOverrideLeftBounds
      case d:Double => d
      //case _ => possiblyOverrideLeftBounds
    }
    val thisMdTop = yOrigin match {
      case Double.NaN => possiblyOverrideTopBounds
      case d:Double => d
      //case _ => possiblyOverrideTopBounds
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
  def generateChanges(rawPublicHistory:History,rawPrivateHistory:History):Tuple2[List[MeTLStanza],List[MeTLStanza]] = Stopwatch.time("MeTLMoveDelta.generateChanges",{
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
  def empty = MeTLMoveDelta(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","",0.0,0.0,Nil,Nil,Nil,0.0,0.0,1.0,1.0,Privacy.NOT_SET,false,Nil)
}

case class MeTLDirtyInk(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val audiences:List[Audience] = Nil) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences) {
  override def matches(other:MeTLCanvasContent) = other match {
    case o:MeTLDirtyInk => super.matches(o)
    case _ => false
  }
  override def isDirtierFor(other:MeTLCanvasContent) = other match {
    case o:MeTLInk => super.matches(o) && o.timestamp < timestamp
    case _ => false
  }
  override def alterPrivacy(newPrivacy:Privacy):MeTLDirtyInk = copy(privacy = newPrivacy)//MeTLDirtyInk(server,author,timestamp,target,newPrivacy,slide,identity)
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLDirtyInk = Stopwatch.time("MeTLDirtyInk.adjustTimestamp",{
    copy(timestamp = newTime)//MeTLDirtyInk(server,author,newTime,target,privacy,slide,identity)
  })
  override def generateNewIdentity(descriptor:String):MeTLDirtyInk = copy(identity = genNewIdentity("newMeTLDirtyInk:"+descriptor))//MeTLDirtyInk(server,author,timestamp,target,privacy,slide,genNewIdentity("newMeTLDirtyInk:"+descriptor))
}
object MeTLDirtyInk{
  def empty = MeTLDirtyInk(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","",Nil)
}

case class MeTLDirtyText(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val audiences:List[Audience] = Nil) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences){
  override def matches(other:MeTLCanvasContent) = other match {
    case o:MeTLDirtyText => super.matches(o)
    case _ => false
  }
  override def isDirtierFor(other:MeTLCanvasContent) = other match {
    case o:MeTLText => super.matches(o) && o.timestamp < timestamp
    case _ => false
  }
  override def alterPrivacy(newPrivacy:Privacy):MeTLDirtyText = copy(privacy=newPrivacy)//MeTLDirtyText(server,author,timestamp,target,newPrivacy,slide,identity)
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLDirtyText = Stopwatch.time("MeTLDirtyText.adjustTimestamp", {
    copy(timestamp=newTime)//MeTLDirtyText(server,author,newTime,target,privacy,slide,identity)
  })
  override def generateNewIdentity(descriptor:String):MeTLDirtyText = copy(identity=genNewIdentity("newMeTLDirtyText:"+descriptor))//MeTLDirtyText(server,author,timestamp,target,privacy,slide,genNewIdentity("newMeTLDirtyText:"+descriptor))
}
object MeTLDirtyText{
  def empty = MeTLDirtyText(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","",Nil)
}

case class MeTLDirtyImage(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,override val target:String,override val privacy:Privacy,override val slide:String,override val identity:String,override val audiences:List[Audience] = Nil) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slide,identity,audiences) {
  override def matches(other:MeTLCanvasContent) = other match {
    case o:MeTLDirtyImage => super.matches(o)
    case _ => false
  }
  override def isDirtierFor(other:MeTLCanvasContent) = other match {
    case o:MeTLImage => super.matches(o) && o.timestamp < timestamp
    case _ => false
  }
  override def alterPrivacy(newPrivacy:Privacy):MeTLDirtyImage = copy(privacy=newPrivacy)//MeTLDirtyImage(server,author,timestamp,target,newPrivacy,slide,identity)
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLDirtyImage = Stopwatch.time("MeTLDirtyImage.adjustTimestamp",{
    copy(timestamp=newTime)//MeTLDirtyImage(server,author,newTime,target,privacy,slide,identity)
  })
  override def generateNewIdentity(descriptor:String):MeTLDirtyImage = copy(identity=genNewIdentity("newMeTLDirtyImage:"+descriptor))//MeTLDirtyImage(server,author,timestamp,target,privacy,slide,genNewIdentity("newMeTLDirtyImage:"+descriptor))
}
object MeTLDirtyImage{
  def empty = MeTLDirtyImage(ServerConfiguration.empty,"",0L,"",Privacy.NOT_SET,"","",Nil)
}

case class MeTLCommand(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,command:String,commandParameters:List[String],override val audiences:List[Audience] = Nil) extends MeTLStanza(server,author,timestamp,audiences){
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLCommand = Stopwatch.time("MeTLCommand.adjustTimestamp",{
    copy(timestamp = newTime)//MeTLCommand(server,author,newTime,command,commandParameters)
  })
}
object MeTLCommand{
  def empty = MeTLCommand(ServerConfiguration.empty,"",0L,"/No_Command",List.empty[String],Nil)
}

case class MeTLQuiz(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,created:Long,question:String,id:String,url:Box[String],imageBytes:Box[Array[Byte]],isDeleted:Boolean,options:List[QuizOption],override val audiences:List[Audience] = Nil) extends MeTLStanza(server,author,timestamp){
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLQuiz = Stopwatch.time("MeTLQuiz.adjustTimestamp",{
    copy(timestamp = newTime)
  })
  def replaceQuestion(newQ:String) = copy(question = newQ)
  def addOption(newO:QuizOption) = copy(options = options ::: List(newO.adjustName(QuizOption.nextName(options))))
  def replaceImage(newImageUrl:Box[String]) = copy(url = newImageUrl,imageBytes = Empty)
  def replaceOption(optionName:String,newText:String) = {
    options.find(o => o.name == optionName).map(or => copy(options = options.filterNot(_ == or) ::: List(or.adjustText(newText)))).getOrElse(copy())
  }
  def removeOption(optionName:String) = {
    options.find(_.name == optionName).map(or => copy(options = options.filterNot(o => o == or).foldLeft(List.empty[QuizOption])((os,o)=> o.adjustName(QuizOption.nextName(os)) :: os))).getOrElse(copy())
  }
  def delete = copy(isDeleted = true)
}
object MeTLQuiz{
  def empty = MeTLQuiz(ServerConfiguration.empty,"",0L,0L,"","",Empty,Empty,true,List.empty[QuizOption],Nil)
}

case class MeTLSubmission(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,title:String,slideJid:Int,url:String,imageBytes:Box[Array[Byte]] = Empty,blacklist:List[SubmissionBlacklistedPerson] = List.empty[SubmissionBlacklistedPerson], override val target:String = "submission",override val privacy:Privacy = Privacy.PUBLIC,override val identity:String = new Date().getTime.toString,override val audiences:List[Audience] = Nil) extends MeTLCanvasContent(server,author,timestamp,target,privacy,slideJid.toString,identity){
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLSubmission = Stopwatch.time("MeTLSubmission.adjustTimestamp",{
    copy(timestamp = newTime)
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
  def adjustName(newName:String) = copy(name = newName,color = QuizOption.colorForName(newName))
  def adjustText(newText:String) = copy(text = newText)
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

case class MeTLQuizResponse(override val server:ServerConfiguration,override val author:String,override val timestamp:Long,answer:String,answerer:String,id:String,override val audiences:List[Audience] = Nil) extends MeTLStanza(server,author,timestamp,audiences){
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLQuizResponse = Stopwatch.time("MeTLQuizResponse.adjustTimestamp",{
      copy(timestamp = newTime)
  })
}
object MeTLQuizResponse{
  def empty = MeTLQuizResponse(ServerConfiguration.empty,"",0L,"","","",Nil)
}

case class MeTLFile(override val server:ServerConfiguration, override val author:String, override val timestamp:Long, name:String, id:String, url:Option[String],bytes:Option[Array[Byte]],override val audiences:List[Audience] = Nil) extends MeTLStanza(server,author,timestamp,audiences){
  override def adjustTimestamp(newTime:Long = new java.util.Date().getTime):MeTLFile = Stopwatch.time("MeTLFile.adjustTimestamp",{
    copy(timestamp = newTime)
  })
}
object MeTLFile{
  def empty = MeTLFile(ServerConfiguration.empty,"",0L,"","",None,None,Nil)
}
