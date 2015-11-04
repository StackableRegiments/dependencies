package com.metl.data

import com.metl.utils._

import net.liftweb.common._
import net.liftweb.util.Helpers._
import Privacy._
import net.liftweb.json.{Serialization, NoTypeHints, TypeInfo, Formats, MappingException}
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

class PrivacySerializer extends net.liftweb.json.Serializer[Privacy] {
    private val PrivacyClass = classOf[Privacy]

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Privacy] = {
      case (TypeInfo(PrivacyClass, _), json) => json match {
        case JString(p) => Privacy.parse(p)
        case x => throw new MappingException("Can't convert " + x + " to Privacy")
      }
    }

    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case x: Privacy => JField("privacy", JString(x.toString.toLowerCase))
    }
}

trait JsonSerializerHelper {

  lazy implicit val formats = Serialization.formats(NoTypeHints) + new PrivacySerializer

  def getStringByName(input:JObject,name:String) = (input \ name).extract[String]
  def getBooleanByName(input:JObject,name:String) = (input \ name).extract[Boolean]
  def getIntByName(input:JObject,name:String) = (input \ name).extract[Int]
  def getLongByName(input:JObject,name:String) = (input \ name).extract[Long]
  def getDoubleByName(input:JObject,name:String) = (input \ name).extract[Double]
  def getPrivacyByName(input:JObject,name:String) = (input \ name).extract[Privacy]
  def getObjectByName(input:JObject,name:String) = input.values(name).asInstanceOf[JObject]
  def getListOfDoublesByName(input:JObject,name:String) = (input \ name).extract[List[Double]]
  def getListOfStringsByName(input:JObject,name:String) = (input \ name).extract[List[String]]
  def getListOfObjectsByName(input:JObject,name:String) = input.values(name).asInstanceOf[List[AnyRef]].map(i => i.asInstanceOf[JObject])
  def getColorByName(input:JObject,name:String) = input.values(name).asInstanceOf[List[Any]]
}

class JsonSerializer(configName:String) extends Serializer with JsonSerializerHelper {
  type T = JValue
  lazy val config = ServerConfiguration.configForName(configName)

  protected def parseAudiences(input:MeTLData):List[JField] = {
    List(
      JField("audiences",JArray(input.audiences.map(a => {
        JObject(List(
          JField("domain",JString(a.domain)),
          JField("name",JString(a.name)),
          JField("type",JString(a.audienceType)),
          JField("action",JString(a.action))
        ))
      })))
    )
  }

  protected def parseMeTLContent(input:MeTLStanza):List[JField] = {
    List(
      JField("author",JString(input.author)),
      JField("timestamp",JInt(input.timestamp))
    ) ::: parseAudiences(input)
  }
  protected def parseCanvasContent(input:MeTLCanvasContent):List[JField] = {
    List(
      JField("target",JString(input.target)),
      JField("privacy",JString(input.privacy.toString)),
      JField("slide",JString(input.slide)),
      JField("identity",JString(input.identity))
    )
  }
  protected def parseJObjForAudiences(input:JObject,config:ServerConfiguration = ServerConfiguration.empty):List[Audience] = {
    ((input \ "audiences").extract[List[JObject]]).flatMap(a => {
      try {
        val domain = getStringByName(a,"domain")
        val name = getStringByName(a,"name")
        val audienceType = getStringByName(a,"type")
        val action = getStringByName(a,"action")
        Some(Audience(config,domain,name,audienceType,action))
      } catch {
        case e:Exception => {
          None
        }
      }
    })
  }

  protected def parseJObjForMeTLContent(input:JObject,config:ServerConfiguration = ServerConfiguration.empty):ParsedMeTLContent = {
    val author = (input \ "author").extract[String] //getStringByName(input,"author")
    val timestamp = getLongByName(input,"timestamp")
    val audiences = parseJObjForAudiences(input,config)
    ParsedMeTLContent(author,timestamp,audiences)
  }
  protected def parseJObjForCanvasContent(input:JObject):ParsedCanvasContent = {
    val target = getStringByName(input,"target")
    val privacy = getPrivacyByName(input,"privacy")
    val slide = getStringByName(input,"slide")
    val identity = getStringByName(input,"identity")
    ParsedCanvasContent(target,privacy,slide,identity)
  }
  override def fromHistory(input:History):JValue = Stopwatch.time("JsonSerializer.fromHistory",() => {
		val (texts,highlighters,inks,images) = input.getRenderableGrouped
    toJsObj("history",List(
      JField("jid",JString(input.jid)),
      JField("inks",JObject(inks.map(i => JField(i.identity,fromMeTLInk(i))))),
      JField("highlighters",JObject(highlighters.map(i => JField(i.identity,fromMeTLInk(i))))),
      JField("images",JObject(images.map(i => JField(i.identity,fromMeTLImage(i))))),
      JField("texts",JObject(texts.map(i => JField(i.identity,fromMeTLText(i))))),
      JField("quizzes",JArray(input.getQuizzes.map(i => fromMeTLQuiz(i)))),
      JField("quizResponses",JArray(input.getQuizResponses.map(i => fromMeTLQuizResponse(i)))),
      JField("submissions",JArray(input.getSubmissions.map(i => fromSubmission(i)))),
      JField("attendances",JArray(input.getAttendances.map(i => fromMeTLAttendance(i)))),
      JField("commands",JArray(input.getCommands.map(i => fromMeTLCommand(i))))
    ))
  })
  protected def hasField(input:JObject,fieldName:String) = Stopwatch.time("JsonSerializer.has", () => {
    input.values.contains(fieldName)
  })
  protected def hasFields(input:JObject,fieldNames:List[String]) = Stopwatch.time("JsonSerializer.hasFields",() => {
    val allValues = input.values
    !fieldNames.exists(fn => !allValues.contains(fn))
  })
  protected def isOfType(input:JObject,name:String) = Stopwatch.time("JsonSerializer.isOfType", () => {
    input.values("type") == name
  })
  protected def toJsObj(name:String,fields:List[JField]) = Stopwatch.time("JsonSerializer.toJsObj", () => {
    JObject(JField("type",JString(name)) :: fields)
  })
  override def toMeTLData(input:JValue):MeTLData = Stopwatch.time("JsonSerializer.toMeTLStanza", () => {
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
      case jo:JObject if (isOfType(jo,"moveDelta")) => toMeTLMoveDelta(jo)
      case jo:JObject if (isOfType(jo,"command")) => toMeTLCommand(jo)
      case jo:JObject if (isOfType(jo,"attendance")) => toMeTLAttendance(jo)
      case other:JObject if hasFields(other,List("target","privacy","slide","identity")) => toMeTLUnhandledCanvasContent(other)
      case other:JObject if hasFields(other,List("author","timestamp")) => toMeTLUnhandledStanza(other)
      case other:JObject => toMeTLUnhandledData(other)
      case nonObjectData => toMeTLUnhandledData(nonObjectData)
    }
  })
  override def fromMeTLUnhandledData(i:MeTLUnhandledData[JValue]):JValue = i.unhandled
  override def fromMeTLUnhandledStanza(i:MeTLUnhandledStanza[JValue]):JValue = i.unhandled
  override def fromMeTLUnhandledCanvasContent(i:MeTLUnhandledCanvasContent[JValue]):JValue = i.unhandled
  override def toMeTLUnhandledData(i:JValue) = MeTLUnhandledData(config,i)
  override def toMeTLUnhandledStanza(i:JValue) = {
    i match {
      case input:JObject => {
        val m = parseJObjForMeTLContent(input,config)
        MeTLUnhandledStanza(config,m.author,m.timestamp,i,m.audiences)
      }
      case other => MeTLUnhandledStanza.empty[JValue](other)
    }
  }

  override def toMeTLUnhandledCanvasContent(i:JValue) = {
    i match {
      case input:JObject => {
        val cc = parseJObjForCanvasContent(input)
        val m = parseJObjForMeTLContent(input,config)
        MeTLUnhandledCanvasContent(config,m.author,m.timestamp,cc.target,cc.privacy,cc.slide,cc.identity,m.audiences,1.0,1.0,i)
      }
      case other => MeTLUnhandledCanvasContent.empty[JValue](other)
    }
  }


  override def fromMeTLMoveDelta(input:MeTLMoveDelta):JValue = Stopwatch.time("JsonSerializer.fromMeTLMoveDelta",()=>{
    toJsObj("moveDelta",List(
      JField("inkIds",JArray(input.inkIds.map(JString).toList)),
      JField("imageIds",JArray(input.imageIds.map(JString).toList)),
      JField("textIds",JArray(input.textIds.map(JString).toList)),
      JField("xTranslate",JDouble(input.xTranslate)),
      JField("yTranslate",JDouble(input.yTranslate)),
      JField("xScale",JDouble(input.xScale)),
      JField("yScale",JDouble(input.yScale)),
      JField("newPrivacy",JString(input.newPrivacy.toString.toLowerCase)),
      JField("isDeleted",JBool(input.isDeleted)),
			JField("xOrigin", JDouble(input.xOrigin)),
			JField("yOrigin", JDouble(input.yOrigin))
    ) ::: parseMeTLContent(input) ::: parseCanvasContent(input))
  })
  override def toMeTLMoveDelta(i:JValue):MeTLMoveDelta = Stopwatch.time("JsonSerializer.toMeTLMoveDelta",()=>{
    i match{
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input,config)
        val cc = parseJObjForCanvasContent(input)
        val inkIds = getListOfStringsByName(input,"inkIds")
        val textIds = getListOfStringsByName(input,"textIds")
        val imageIds = getListOfStringsByName(input,"imageIds")
        val xTranslate = getDoubleByName(input,"xTranslate")
        val yTranslate = getDoubleByName(input,"yTranslate")
        val xScale = getDoubleByName(input,"xScale")
        val yScale = getDoubleByName(input,"yScale")
        val newPrivacy = getPrivacyByName(input,"newPrivacy")
        val isDeleted = getBooleanByName(input,"isDeleted")
				val xOrigin = getDoubleByName(input,"xOrigin")
				val yOrigin = getDoubleByName(input,"yOrigin")
        MeTLMoveDelta(config,mc.author,mc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity,xOrigin,yOrigin,inkIds,textIds,imageIds,xTranslate,yTranslate,xScale,yScale,newPrivacy,isDeleted,mc.audiences)
      }
      case _ => MeTLMoveDelta.empty
    }
  })
  override def toMeTLAttendance(i:JValue):Attendance = Stopwatch.time("JsonSerializer.toMeTLAttendance",() => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input,config)
        val location = getStringByName(input,"location")
        val present = getBooleanByName(input,"present")
        Attendance(config,mc.author,mc.timestamp,location,present,mc.audiences)
      }
      case _ => Attendance.empty
    }
  })
  override def fromMeTLAttendance(i:Attendance):JValue = Stopwatch.time("JsonSerializer.fromMeTLAttendance",() => {
    toJsObj("attendance",List(
      JField("location",JString(i.location)),
      JField("present",JBool(i.present))
      ) ::: parseMeTLContent(i))
  })
  override def toMeTLInk(i:JValue):MeTLInk = Stopwatch.time("JsonSerializer.toMeTLInk", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input,config)
        val cc = parseJObjForCanvasContent(input)
        val checksum = getDoubleByName(input,"checksum")
        val startingSum = getDoubleByName(input,"startingSum")
        val points = toPointList(getListOfDoublesByName(input,"points")).asInstanceOf[List[Point]]
        val color = toColor(getColorByName(input,"color"))
        val thickness = getDoubleByName(input,"thickness")
        val isHighlighter = getBooleanByName(input,"isHighlighter")
        MeTLInk(config,mc.author,mc.timestamp,checksum,startingSum,points,color,thickness,isHighlighter,cc.target,cc.privacy,cc.slide,cc.identity,mc.audiences)
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
        val mc = parseJObjForMeTLContent(input,config)
        val cc = parseJObjForCanvasContent(input)
        val tag = getStringByName(input,"tag")
        val source = Full(getStringByName(input,"source"))
        val imageBytes = source.map(u => config.getResource(u))
        val pngBytes = Empty
        val width = getDoubleByName(input,"width")
        val height = getDoubleByName(input,"height")
        val x = getDoubleByName(input,"x")
        val y = getDoubleByName(input,"y")
        MeTLImage(config,mc.author,mc.timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,cc.target,cc.privacy,cc.slide,cc.identity,mc.audiences)
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
        val mc = parseJObjForMeTLContent(input,config)
        val cc = parseJObjForCanvasContent(input)
        val text = getStringByName(input,"text")
        val height = getDoubleByName(input,"height")
        val width = getDoubleByName(input,"width")
        val caret = getIntByName(input,"caret")
        val x = getDoubleByName(input,"x")
        val y = getDoubleByName(input,"y")
        val tag = getStringByName(input,"tag")
        val style = getStringByName(input,"style")
        val family = getStringByName(input,"family")
        val weight = getStringByName(input,"weight")
        val size = getDoubleByName(input,"size")
        val decoration = getStringByName(input,"decoration")
        val color = toColor(getColorByName(input,"color"))
        MeTLText(config,mc.author,mc.timestamp,text,height,width,caret,x,y,tag,style,family,weight,size,decoration,cc.identity,cc.target,cc.privacy,cc.slide,color,mc.audiences)
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
        val mc = parseJObjForMeTLContent(input,config)
        val cc = parseJObjForCanvasContent(input)
        MeTLDirtyInk(config,mc.author,mc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity,mc.audiences)
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
        val mc = parseJObjForMeTLContent(input,config)
        val cc = parseJObjForCanvasContent(input)
        MeTLDirtyImage(config,mc.author,mc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity,mc.audiences)
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
        val mc = parseJObjForMeTLContent(input,config)
        val cc = parseJObjForCanvasContent(input)
        MeTLDirtyText(config,mc.author,mc.timestamp,cc.target,cc.privacy,cc.slide,cc.identity,mc.audiences)
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
        val mc = parseJObjForMeTLContent(input,config)
        val command = getStringByName(input,"command")
        val parameters = getListOfStringsByName(input,"parameters")
        MeTLCommand(config,mc.author,mc.timestamp,command,parameters,mc.audiences)
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
        val mc = parseJObjForMeTLContent(input,config)
				val cc = parseJObjForCanvasContent(input)
        val slide = getIntByName(input,"slide")
        val url = getStringByName(input,"url")
				val title = getStringByName(input,"title")
				val blacklist = getListOfObjectsByName(input,"blacklist").map(blo => {
					val username = getStringByName(input,"username")
					val highlight = toColor(getColorByName(input,"highlight"))
					SubmissionBlacklistedPerson(username,highlight)
				}).toList
        MeTLSubmission(config,mc.author,mc.timestamp,title,slide,url,Empty,blacklist,cc.target,cc.privacy,cc.identity,mc.audiences)
      }
      case _ => MeTLSubmission.empty
    }
  })
  override def fromSubmission(input:MeTLSubmission):JValue = Stopwatch.time("JsonSerializer.fromSubmission",() => {
    toJsObj("submission",List(
      JField("url",JString(input.url)),
			JField("title",JString(input.title)),
			JField("blacklist",JArray(input.blacklist.map(bl => JObject(List(JField("username",JString(bl.username)),JField("highlight",fromColor(bl.highlight).asInstanceOf[JValue]))))))
    ) ::: parseMeTLContent(input) ::: parseCanvasContent(input))
  })
  override def toMeTLQuiz(i:JValue):MeTLQuiz = Stopwatch.time("JsonSerializer.toMeTLQuiz", () => {
    i match {
      case input:JObject => {
        val mc = parseJObjForMeTLContent(input,config)
        val created = getLongByName(input,"created")
        val question = getStringByName(input,"question")
        val id = getStringByName(input,"id")
        val isDeleted = getBooleanByName(input,"isDeleted")
        val options = getListOfObjectsByName(input,"options").map(o => toQuizOption(o))
        val url = tryo(getStringByName(input,"url"))
        val quizImage = url.map(u =>config.getResource(u))
        MeTLQuiz(config,mc.author,mc.timestamp,created,question,id,url,quizImage,isDeleted,options,mc.audiences)
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
        val name = getStringByName(input,"name")
        val text = getStringByName(input,"text")
        val correct = getBooleanByName(input,"text")
        val color = toColor(getColorByName(input,"color"))
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
        val mc = parseJObjForMeTLContent(input,config)
        val answer = getStringByName(input,"answer")
        val answerer = getStringByName(input,"answerer")
        val id = getStringByName(input,"id")
        MeTLQuizResponse(config,mc.author,mc.timestamp,answer,answerer,id,mc.audiences)
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
        val author = getStringByName(input,"author")
        val lastAccessed = getLongByName(input,"lastAccessed")
        val slides = getListOfObjectsByName(input,"slides").map(s => toSlide(s)).toList
        val subject = getStringByName(input,"subject")
        val tag = getStringByName(input,"tag")
        val jid = getIntByName(input,"jid")
        val title = getStringByName(input,"title")
        val created = getStringByName(input,"created")
        val permissions = toPermissions(getObjectByName(input,"permissions"))
				val thisConfig = getStringByName(input,"configName") match {
					case "" => config
					case other => ServerConfiguration.configForName(other)
				}
        Conversation(thisConfig,author,lastAccessed,slides,subject,tag,jid,title,created,permissions)
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
      JField("permissions",fromPermissions(input.permissions)),
			JField("configName",JString(input.server.name))
    ))
  })
  override def toSlide(i:JValue):Slide = Stopwatch.time("JsonSerializer.toSlide", () => {
    i match {
      case input:JObject => {
        val author = getStringByName(input,"author")
        val id = getIntByName(input,"id")
        val index = getIntByName(input,"index")
        val defaultHeight = getIntByName(input,"defaultHeight")
        val defaultWidth = getIntByName(input,"defaultWidth")
        val exposed = getBooleanByName(input,"exposed")
        val slideType = getStringByName(input,"slideType")
        val groupSet = getListOfObjectsByName(input,"groupSet").map(gs => toGroupSet(gs))
        Slide(config,author,id,index,defaultHeight,defaultWidth,exposed,slideType,groupSet)
      }
      case _ => Slide.empty
    }
  })
  override def fromSlide(input:Slide):JValue = Stopwatch.time("JsonSerializer.fromSlide",() => {
    JObject(List(
      JField("id",JInt(input.id)),
      JField("author",JString(input.author)),
      JField("index",JInt(input.index)),
      JField("defaultHeight",JInt(input.defaultHeight)),
      JField("defaultWidth",JInt(input.defaultWidth)),
      JField("exposed",JBool(input.exposed)),
      JField("slideType",JString(input.slideType))
    ) ::: List(input.groupSet.map(gs => JField("groupSet",fromGroupSet(gs)))).flatten)
  })
 override def toGroupSet(i:JValue):GroupSet = Stopwatch.time("JsonSerializer.toGroupSet",() => {
   i match {
     case input:JObject => {
        val audiences = parseJObjForAudiences(input)
        val id = getStringByName(input,"id")
        val location = getStringByName(input,"location")
        val groupingStrategy = toGroupingStrategy((input \ "groupingStrategy").extract[JObject])
        val groups = getListOfObjectsByName(input,"groups").map(gn => toGroup(gn))
        GroupSet(config,id,location,groupingStrategy,groups,audiences)
     }
     case _ => GroupSet.empty
   }
  })
  override def fromGroupSet(input:GroupSet):JValue = Stopwatch.time("JsonSerializer.fromGroupSet",() => {
    toJsObj("groupSet",List(
      JField("id",JString(input.id)),
      JField("location",JString(input.location)),
      JField("groupingStrategy",fromGroupingStrategy(input.groupingStrategy)),
      JField("groups",JArray(input.groups.map(g => fromGroup(g)).toList))
    ) ::: parseAudiences(input))
  })

  override def toGroupingStrategy(i:JValue):GroupingStrategy = Stopwatch.time("JsonSerializer.toGroupingStrategy",() => {
    i match {
      case input:JObject => {
        getStringByName(input,"name") match {
          case "byMaximumSize" => ByMaximumSize(getIntByName(input,"groupSize"))
          case "byTotalGroups" => ByTotalGroups(getIntByName(input,"groupCount"))
          case "onePersonPerGroup" => OnePersonPerGroup
          case "everyoneInOneGroup" => EveryoneInOneGroup
          case "complexGroupingStrategy" => ComplexGroupingStrategy(Map("json" -> (input \ "data").extract[JObject].toString)) // let's make this actually read the JFields of the JObject at input \ data and put them recursiely into a Map.
          case _ => EveryoneInOneGroup
        }
      }
      case _ => EveryoneInOneGroup
    }
  })
  override def fromGroupingStrategy(input:GroupingStrategy):JValue = Stopwatch.time("JsonSerializer.fromGroupingStrategy",() => {
    val contents = input match {
      case ByMaximumSize(groupSize) => List(JField("name",JString("byMaximumSize")),JField("groupSize",JInt(groupSize)))
      case ByTotalGroups(groupCount) => List(JField("name",JString("byTotalGroups")),JField("groupCount",JInt(groupCount)))
      case OnePersonPerGroup => List(JField("name",JString("onePersonPerGroup")))
      case EveryoneInOneGroup => List(JField("name",JString("everyoneInOneGroup")))
      case ComplexGroupingStrategy(data) => List(JField("name",JString("complexGroupingStrategy")),JField("data",JString(data.toString))) // let's serialize this more strongly into a recursive JObject
      case _ => List(JField("name",JString("unknown")))
    }
    toJsObj("groupingStrategy",contents)
  })

  override def toGroup(i:JValue):Group = Stopwatch.time("JsonSerializer.toGroup",() => {
    i match {
      case input:JObject => {
        val audiences = parseJObjForAudiences(input,config)
        val id = getStringByName(input,"id")
        val location = getStringByName(input,"location")
        val members = getListOfStringsByName(input,"members")
        Group(config,id,location,members,audiences)
      }
      case _ => Group.empty
    }
  })
  override def fromGroup(input:Group):JValue = Stopwatch.time("JsonSerializer.fromGroup",() => {
    toJsObj("group",List(
      JField("id",JString(input.id)),
      JField("location",JString(input.location)),
      JField("members",JArray(input.members.map(m => JString(m))))
    ) ::: parseAudiences(input))
  })
  override def toPermissions(i:JValue):Permissions = Stopwatch.time("JsonSerializer.toPermissions", () => {
    i match {
      case input:JObject => {
        val studentsCanOpenFriends = getBooleanByName(input,"studentCanOpenFriends")
        val studentsCanPublish = getBooleanByName(input,"studentCanPublish")
        val usersAreCompulsorilySynced = getBooleanByName(input,"usersAreCompulsorilySynced")
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
  protected def convert2AfterN(h:String,n:Int):Int = hexToInt(h.drop(n).take(2).mkString)
  protected def hexToInt(h:String):Int = tryo(Integer.parseInt(h,16)).openOr(0)
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

