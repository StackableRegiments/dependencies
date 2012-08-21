package com.metl.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import Privacy._
import net.liftweb.json.JsonAST._

object ConversionHelper {
  def toDouble(a:Any):Double = a match{
    case d:Double => d
    case i:scala.math.BigInt => i.doubleValue
    case JString("NaN") => Double.NaN
    case other =>{
      println("Attempted to apply ConversionHelper.toDouble to [%s]".format(other))
      Double.NaN
    }
  }
}
class JsonSerializerHelper {
  def getStringByName(input:JObject,name:String) = input.values(name).asInstanceOf[String]
  def getBooleanByName(input:JObject,name:String) = input.values(name).asInstanceOf[Boolean]
  def getIntByName(input:JObject,name:String) = ConversionHelper.toDouble(input.values(name)).intValue
  def getLongByName(input:JObject,name:String) = ConversionHelper.toDouble(input.values(name)).longValue
  def getDoubleByName(input:JObject,name:String) = ConversionHelper.toDouble(input.values(name))
  def getPrivacyByName(input:JObject,name:String) = Privacy.parse(input.values(name).asInstanceOf[String])
  def getObjectByName(input:JObject,name:String) = input.values(name).asInstanceOf[JObject]
  def getListOfDoublesByName(input:JObject,name:String) = input.values(name).asInstanceOf[List[AnyRef]].map(ConversionHelper.toDouble)
  def getListOfStringsByName(input:JObject,name:String) = input.values(name).asInstanceOf[List[AnyRef]].map(i => i.asInstanceOf[String])
  def getListOfObjectsByName(input:JObject,name:String) = input.values(name).asInstanceOf[List[AnyRef]].map(i => i.asInstanceOf[JObject])
  def getColorByName(input:JObject,name:String) = input.values(name).asInstanceOf[List[Any]]
}

class JsonSerializer(configName:String) extends Serializer{
  type T = JValue
  lazy val config = ServerConfiguration.configForName(configName)
  val utils = new JsonSerializerHelper

  private def parseMeTLContent(input:MeTLStanza):List[JField] = {
    List(
      JField("author",JString(input.author)),
      JField("timestamp",JInt(input.timestamp))
    )
  }
  private def parseCanvasContent(input:MeTLCanvasContent):List[JField] = {
    List(
      JField("target",JString(input.target)),
      JField("privacy",JString(input.privacy.toString)),
      JField("slide",JString(input.slide)),
      JField("identity",JString(input.identity))
    )
  }
  private def parseJObjForMeTLContent(input:JObject):ParsedMeTLContent = {
    val author = utils.getStringByName(input,"author")
    val timestamp = utils.getLongByName(input,"timestamp")
    ParsedMeTLContent(author,timestamp)
  }
  private def parseJObjForCanvasContent(input:JObject):ParsedCanvasContent = {
    val target = utils.getStringByName(input,"target")
    val privacy = utils.getPrivacyByName(input,"privacy")
    val slide = utils.getStringByName(input,"slide")
    val identity = utils.getStringByName(input,"identity")
    ParsedCanvasContent(target,privacy,slide,identity)
  }
  override def fromHistory(input:History):JValue = Stopwatch.time("JsonSerializer.fromHistory",() => {
    toJsObj("history",List(
      JField("jid",JString(input.jid)),
      JField("inks",JObject(input.getInks.map(i => JField(i.identity,fromMeTLInk(i))))),
      JField("highlighters",JObject(input.getHighlighters.map(i => JField(i.identity,fromMeTLInk(i))))),
      JField("images",JObject(input.getImages.map(i => JField(i.identity,fromMeTLImage(i))))),
      JField("texts",JObject(input.getTexts.map(i => JField(i.identity,fromMeTLText(i))))),
      JField("quizzes",JArray(input.getQuizzes.map(i => fromMeTLQuiz(i)))),
      JField("quizResponses",JArray(input.getQuizResponses.map(i => fromMeTLQuizResponse(i)))),
      JField("submissions",JArray(input.getSubmissions.map(i => fromSubmission(i)))),
      JField("commands",JArray(input.getCommands.map(i => fromMeTLCommand(i))))
    ))
  })
  private def hasField(input:JObject,fieldName:String) = Stopwatch.time("JsonSerializer.has", () => {
    input.values.contains(fieldName)
  })
  private def isOfType(input:JObject,name:String) = Stopwatch.time("JsonSerializer.isOfType", () => {
    input.values("type") == name
  })
  private def toJsObj(name:String,fields:List[JField]) = Stopwatch.time("JsonSerializer.toJsObj", () => {
    JObject(JField("type",JString(name)) :: fields)
  })
  override def toMeTLStanza(input:JValue):MeTLStanza = Stopwatch.time("JsonSerializer.toMeTLStanza", () => {
    input match {
      case jo:JObject if (isOfType(jo,"ink")) => toMeTLInk(jo)
      case jo:JObject if (isOfType(jo,"text")) => toMeTLText(jo)
      case jo:JObject if (isOfType(jo,"image")) => toMeTLImage(jo)
      case jo:JObject if (isOfType(jo,"dirtyInk")) => toMeTLDirtyInk(jo)
      case jo:JObject if (isOfType(jo,"dirtyText")) => toMeTLDirtyText(jo)
      case jo:JObject if (isOfType(jo,"dirtyImage")) => toMeTLDirtyImage(jo)
      case jo:JObject if (isOfType(jo,"submission")) => toSubmission(jo)
      case jo:JObject if (isOfType(jo,"quiz")) => toMeTLQuiz(jo)
      case jo:JObject if (isOfType(jo,"quizResponse")) => toMeTLQuizResponse(jo)
      case _ => MeTLStanza.empty
    }
  })
  override def toMeTLInk(i:JValue):MeTLInk = Stopwatch.time("JsonSerializer.toMeTLInk", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input)
        val cc = parseJObjForCanvasContent(input)
        val checksum = utils.getDoubleByName(input,"checksum")
        val startingSum = utils.getDoubleByName(input,"startingSum")
        val points = toPointList(utils.getListOfDoublesByName(input,"points")).asInstanceOf[List[Point]]
        val color = toColor(utils.getColorByName(input,"color"))
        val thickness = utils.getDoubleByName(input,"thickness")
        val isHighlighter = utils.getBooleanByName(input,"isHighlighter")
        MeTLInk(config,mc.author,mc.timestamp,checksum,startingSum,points,color,thickness,isHighlighter,cc.target,cc.privacy,cc.slide,startingSum.toString)
      }
      case _ => MeTLInk.empty
    }
  })
  override def fromMeTLInk(input:MeTLInk):JValue = Stopwatch.time("JsonSerializer.fromMeTLInk",() => {
    toJsObj("ink",List(
      JField("bounds",JArray(List(input.left,input.top,input.right,input.bottom).map(JDouble))),
      JField("checksum",JDouble(input.checksum)),
      JField("startingSum",JDouble(input.startingSum)),
      JField("points",fromPointList(input.points).asInstanceOf[JValue]),
      JField("color",fromColor(input.color).asInstanceOf[JValue]),
      JField("thickness",JDouble(input.thickness)),
      JField("isHighlighter",JBool(input.isHighlighter))
    ) ::: parseMeTLContent(input) ::: parseCanvasContent(input))
  })
  override def toMeTLImage(i:JValue):MeTLImage = Stopwatch.time("JsonSerializer.toMeTLImage", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input)
        val cc = parseJObjForCanvasContent(input)
        val tag = utils.getStringByName(input,"tag")
        val source = Full(utils.getStringByName(input,"source"))
        val imageBytes = source.map(u => config.getResource(u))
        val pngBytes = Empty
        val width = utils.getDoubleByName(input,"width")
        val height = utils.getDoubleByName(input,"height")
        val x = utils.getDoubleByName(input,"x")
        val y = utils.getDoubleByName(input,"y")
        MeTLImage(config,mc.author,mc.timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,cc.target,cc.privacy,cc.slide,cc.identity)
      }
      case _ => MeTLImage.empty
    }
  })
  override def fromMeTLImage(input:MeTLImage):JValue = Stopwatch.time("JsonSerializer.fromMeTLImage",() => {
    toJsObj("image",List(
      JField("tag",JString(input.tag)),
      JField("width",JDouble(if(input.width.isNaN) 0 else input.width)),
      JField("height",JDouble(if(input.height.isNaN) 0 else input.height)),
      JField("x",JDouble(input.x)),
      JField("y",JDouble(input.y))
    ) ::: input.source.map(u => List(JField("source",JString(u)))).openOr(List.empty[JField]) ::: parseMeTLContent(input) ::: parseCanvasContent(input))
  })
  override def toMeTLText(i:JValue):MeTLText = Stopwatch.time("JsonSerializer.toMeTLText", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input)
        val cc = parseJObjForCanvasContent(input)
        val text = utils.getStringByName(input,"text")
        val height = utils.getDoubleByName(input,"height")
        val width = utils.getDoubleByName(input,"width")
        val caret = utils.getIntByName(input,"caret")
        val x = utils.getDoubleByName(input,"x")
        val y = utils.getDoubleByName(input,"y")
        val tag = utils.getStringByName(input,"tag")
        val style = utils.getStringByName(input,"style")
        val family = utils.getStringByName(input,"family")
        val weight = utils.getStringByName(input,"weight")
        val size = utils.getDoubleByName(input,"size")
        val decoration = utils.getStringByName(input,"decoration")
        val color = toColor(utils.getColorByName(input,"color"))
        MeTLText(config,mc.author,mc.timestamp,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,cc.identity,cc.target,cc.privacy,cc.slide,color)
      }
      case _ => MeTLText.empty
    }
  })
  override def fromMeTLText(input:MeTLText):JValue = Stopwatch.time("JsonSerializer.fromMeTLText",() => {
    toJsObj("text",List(
      JField("text",JString(input.text)),
      JField("height",JDouble(input.height)),
      JField("width",JDouble(input.width)),
      JField("x",JDouble(input.x)),
      JField("y",JDouble(input.y)),
      JField("caret",JInt(input.caret)),
      JField("tag",JString(input.tag)),
      JField("style",JString(input.style)),
      JField("family",JString(input.family)),
      JField("font",JString("%spx %s".format(input.size,input.family))),
      JField("weight",JString(input.weight)),
      JField("size",JDouble(input.size)),
      JField("decoration",JString(input.decoration)),
      JField("color",fromColor(input.color).asInstanceOf[JValue])
    ) ::: parseMeTLContent(input) ::: parseCanvasContent(input))
  })
  override def toMeTLDirtyInk(i:JValue):MeTLDirtyInk = Stopwatch.time("JsonSerializer.toMeTLDirtyInk", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input)
        val cc = parseJObjForCanvasContent(input)
        MeTLDirtyInk(config,mc.author,mc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity)
      }
      case _ => MeTLDirtyInk.empty
    }
  })
  override def fromMeTLDirtyInk(input:MeTLDirtyInk):JValue = Stopwatch.time("JsonSerializer.fromMeTLDirtyInk",() => {
    toJsObj("dirtyInk",parseMeTLContent(input) ::: parseCanvasContent(input))
  })
  override def toMeTLDirtyImage(i:JValue):MeTLDirtyImage = Stopwatch.time("JsonSerializer.toMeTLDirtyImage", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input)
        val cc = parseJObjForCanvasContent(input)
        MeTLDirtyImage(config,mc.author,mc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity)
      }
      case _ => MeTLDirtyImage.empty
    }
  })
  override def fromMeTLDirtyImage(input:MeTLDirtyImage):JValue = Stopwatch.time("JsonSerializer.fromMeTLDirtyImage",() => {
    toJsObj("dirtyImage",parseMeTLContent(input) ::: parseCanvasContent(input))
  })
  override def toMeTLDirtyText(i:JValue):MeTLDirtyText = Stopwatch.time("JsonSerializer.toMeTLDirtyText", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input)
        val cc = parseJObjForCanvasContent(input)
        MeTLDirtyText(config,mc.author,mc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity)
      }
      case _ => MeTLDirtyText.empty
    }
  })
  override def fromMeTLDirtyText(input:MeTLDirtyText):JValue = Stopwatch.time("JsonSerializer.fromMeTLDirtyText",() => {
    toJsObj("dirtyText",parseMeTLContent(input) ::: parseCanvasContent(input))
  })
  override def toMeTLCommand(i:JValue):MeTLCommand = Stopwatch.time("JsonSerializer.toMeTLCommand", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input)
        val command = utils.getStringByName(input,"command")
        val parameters = utils.getListOfStringsByName(input,"parameters")
        MeTLCommand(config,mc.author,mc.timestamp,command,parameters)
      }
      case _ => MeTLCommand.empty
    }
  })
  override def fromMeTLCommand(input:MeTLCommand):JValue = Stopwatch.time("JsonSerializer.fromMeTLCommand",() => {
    toJsObj("command",List(
      JField("command",JString(input.command)),
      JField("parameters",JArray(input.commandParameters.map(cp => JString(cp))))
    ) ::: parseMeTLContent(input))
  })
  override def toSubmission(i:JValue):MeTLSubmission = Stopwatch.time("JsonSerializer.toSubmission", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input)
        val slide = utils.getIntByName(input,"slide")
        val url = utils.getStringByName(input,"url")
        MeTLSubmission(config,mc.author,mc.timestamp,slide,url)
      }
      case _ => MeTLSubmission.empty
    }
  })
  override def fromSubmission(input:MeTLSubmission):JValue = Stopwatch.time("JsonSerializer.fromSubmission",() => {
    toJsObj("submission",List(
      JField("slide",JInt(input.slide)),
      JField("url",JString(input.url))
    ) ::: parseMeTLContent(input))
  })
  override def toMeTLQuiz(i:JValue):MeTLQuiz = Stopwatch.time("JsonSerializer.toMeTLQuiz", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input)
        val created = utils.getLongByName(input,"created")
        val question = utils.getStringByName(input,"question")
        val id = utils.getStringByName(input,"id")
        val isDeleted = utils.getBooleanByName(input,"isDeleted")
        val options = utils.getListOfObjectsByName(input,"options").map(o => toQuizOption(o))
        val url = tryo(utils.getStringByName(input,"url"))
        val quizImage = url.map(u =>config.getResource(u))
        MeTLQuiz(config,mc.author,mc.timestamp,created,question,id,url,quizImage,isDeleted,options)
      }
      case _ => MeTLQuiz.empty
    }
  })
  override def fromMeTLQuiz(input:MeTLQuiz):JValue = Stopwatch.time("JsonSerializer.fromMeTLQuiz",() => {
    toJsObj("quiz",List(
      JField("created",JInt(input.created)),
      JField("question",JString(input.question)),
      JField("id",JString(input.id)),
      JField("isDeleted",JBool(input.isDeleted)),
      JField("options",JArray(input.options.map(o => fromQuizOption(o))))
    ) ::: input.url.map(u => List(JField("url",JString(u)))).openOr(List.empty[JField]) ::: parseMeTLContent(input))
  })
  def toQuizOption(i:JValue):QuizOption = Stopwatch.time("JsonSerializer.toQuizOption", () => {
    i match {
      case input:JObject => {
        val name = utils.getStringByName(input,"name")
        val text = utils.getStringByName(input,"text")
        val correct = utils.getBooleanByName(input,"text")
        val color = toColor(utils.getColorByName(input,"color"))
        QuizOption(name,text,correct,color)
      }
      case _ => QuizOption.empty
    }
  })
  def fromQuizOption(input:QuizOption):JValue = Stopwatch.time("JsonSerializer.fromQuizOption", () => {
    toJsObj("quizOption",List(
      JField("name",JString(input.name)),
      JField("text",JString(input.text)),
      JField("correct",JBool(input.correct)),
      JField("color",fromColor(input.color).asInstanceOf[JValue])
    ))
  })
  override def toMeTLQuizResponse(i:JValue):MeTLQuizResponse = Stopwatch.time("JsonSerializer.toMeTLQuizResponse", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input)
        val answer = utils.getStringByName(input,"answer")
        val answerer = utils.getStringByName(input,"answerer")
        val id = utils.getStringByName(input,"id")
        MeTLQuizResponse(config,mc.author,mc.timestamp,answer,answerer,id)
      }
      case _ => MeTLQuizResponse.empty
    }
  })
  override def fromMeTLQuizResponse(input:MeTLQuizResponse):JValue = Stopwatch.time("JsonSerializer.fromMeTLQuizResponse",() => {
    toJsObj("quizResponse",List(
      JField("answer",JString(input.answer)),
      JField("answerer",JString(input.answerer)),
      JField("id",JString(input.id))
    ) ::: parseMeTLContent(input))
  })

  override def toConversation(i:JValue):Conversation = Stopwatch.time("JsonSerializer.toConversation", () => {
    i match {
      case input:JObject => {
        val author = utils.getStringByName(input,"author")
        val lastAccessed = utils.getLongByName(input,"lastAccessed")
        val slides = utils.getListOfObjectsByName(input,"slides").map(s => toSlide(s)).toList
        val subject = utils.getStringByName(input,"subject")
        val tag = utils.getStringByName(input,"tag")
        val jid = utils.getIntByName(input,"jid")
        val title = utils.getStringByName(input,"title")
        val created = utils.getStringByName(input,"created")
        val permissions = toPermissions(utils.getObjectByName(input,"permissions"))
        Conversation(config,author,lastAccessed,slides,subject,tag,jid,title,created,permissions)
      }
      case _ => Conversation.empty
    }
  })
  override def fromConversationList(input:List[Conversation]):JValue = Stopwatch.time("JsonSerializer.fromConversationList",() => JArray(input.map(c => fromConversation(c))))
  override def fromConversation(input:Conversation):JValue = Stopwatch.time("JsonSerializer.fromConversation", () => {
    JObject(List(
      JField("author",JString(input.author)),
      JField("lastAccessed",JInt(input.lastAccessed)),
      JField("slides",JArray(input.slides.map(s => fromSlide(s)).toList)),
      JField("subject",JString(input.subject)),
      JField("tag",JString(input.tag)),
      JField("jid",JInt(input.jid)),
      JField("title",JString(input.title)),
      JField("created",JString(input.created)),
      JField("permissions",fromPermissions(input.permissions))
    ))
  })
  override def toSlide(i:JValue):Slide = Stopwatch.time("JsonSerializer.toSlide", () => {
    i match {
      case input:JObject => {
        val author = utils.getStringByName(input,"author")
        val id = utils.getIntByName(input,"id")
        val index = utils.getIntByName(input,"index")
        Slide(config,author,id,index)
      }
      case _ => Slide.empty
    }
  })
  override def fromSlide(input:Slide):JValue = Stopwatch.time("JsonSerializer.fromSlide",() => {
    JObject(List(
      JField("id",JInt(input.id)),
      JField("author",JString(input.author)),
      JField("index",JInt(input.index))
    ))
  })
  override def toPermissions(i:JValue):Permissions = Stopwatch.time("JsonSerializer.toPermissions", () => {
    i match {
      case input:JObject => {
        val studentsCanOpenFriends = utils.getBooleanByName(input,"studentCanOpenFriends")
        val studentsCanPublish = utils.getBooleanByName(input,"studentCanPublish")
        val usersAreCompulsorilySynced = utils.getBooleanByName(input,"usersAreCompulsorilySynced")
        Permissions(config,studentsCanOpenFriends,studentsCanPublish,usersAreCompulsorilySynced)
      }
      case _ => Permissions.default(config)
    }
  })
  override def fromPermissions(input:Permissions):JValue = Stopwatch.time("JsonSerializer.fromPermissions",() => {
    JObject(List(
      JField("studentCanOpenFriends",JBool(input.studentsCanOpenFriends)),
      JField("studentCanPublish",JBool(input.studentsCanPublish)),
      JField("usersAreCompulsorilySynced",JBool(input.usersAreCompulsorilySynced))
    ))
  })
  private def convert2AfterN(h:String,n:Int):Int = hexToInt(h.drop(n).take(2).mkString)
  private def hexToInt(h:String):Int = tryo(Integer.parseInt(h,16)).openOr(0)
  override def toColor(input:AnyRef):Color = Stopwatch.time("JsonSerializer.toColor", () => {
    input match {
      case List(c,a) => {
        val color = c.asInstanceOf[String]
        val alpha = ConversionHelper.toDouble(a).toInt
        def clamp (n:Integer,min:Integer=0,max:Integer=255) = Math.max(min,Math.min(max,n))
        val r = convert2AfterN(color,1)
        val g = convert2AfterN(color,3)
        val b = convert2AfterN(color,5)
        Color(alpha,clamp(r),clamp(g),clamp(b))
      }
      case _ => Color.empty
    }
  })
  override def fromColor(input:Color):AnyRef = Stopwatch.time("JsonSerializer.fromColor",() => {
    JArray(List(JString("#%02x%02x%02x".format(input.red,input.green,input.blue)),JInt(input.alpha)))
  })
  override def toPointList(input:AnyRef):List[Point] = Stopwatch.time("JsonSerializer.toPointList", () => {
    input match {
      case l:List[Any] if (l.length >= 3) => {
        toPoint(l.take(3)) :: toPointList(l.drop(3))
      }
      case _ => List.empty[Point]
    }
  })
  override def fromPointList(input:List[Point]):AnyRef = Stopwatch.time("JsonSerializer.fromPointList", () => {
    JArray(input.map(p => fromPoint(p).asInstanceOf[List[JValue]]).flatten)
  })
  override def toPoint(input:AnyRef):Point = Stopwatch.time("JsonSerializer.toPoint", () => {
    input match {
      case l:List[Any] if (l.length == 3) => {
        val x = ConversionHelper.toDouble(l(0))
        val y = ConversionHelper.toDouble(l(1))
        val thickness = ConversionHelper.toDouble(l(2))
        Point(x,y,thickness)
      }
      case _ => Point.empty
    }
  })
  override def fromPoint(input:Point):AnyRef = Stopwatch.time("JsonSerializer.fromPoint", () => {
    List(
      JDouble(input.x),
      JDouble(input.y),
      JDouble(input.thickness)
    )
  })
}

