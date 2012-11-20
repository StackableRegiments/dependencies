package com.metl.metl2011

import com.metl.data._
import com.metl.utils._

import scala.xml._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import Privacy._

import javax.imageio._
import java.io.{ByteArrayInputStream,ByteArrayOutputStream}

class MeTL2011XmlSerializer(configName:String,cacheImages:Boolean = false,transcodePng:Boolean = false) extends GenericXmlSerializer(configName){
	
	private val imageCache = new SynchronizedWriteMap[String,Array[Byte]](scala.collection.mutable.HashMap.empty[String,Array[Byte]],true,(k:String) => config.getResource(k))
	private def getCachedImage(url:String) = Stopwatch.time("MeTL2011XmlSerializer.getCachedImage", () => imageCache.getOrElseUpdate(url,config.getResource(url)))
	private val metlUtils = new MeTL2011Utils(configName)
	override def fromMeTLImage(input:MeTLImage):NodeSeq = Stopwatch.time("MeTL2011XmlSerializer.fromMeTLImage",() => {
		val newSource = input.source.map(u => metlUtils.deabsolutizeUri(u,config)).getOrElse(Empty)
		val newImage = MeTLImage(input.server,input.author,input.timestamp,input.tag,newSource,input.imageBytes,input.pngBytes,input.width,input.height,input.x,input.y,input.target,input.privacy,input.slide,input.identity,input.scaleFactorX,input.scaleFactorY)
		//println("serializing newImage for xmpp: %s".format(newImage))
		super.fromMeTLImage(newImage)
	})
	override def toMeTLImage(input:NodeSeq):MeTLImage = Stopwatch.time("MeTL2011XmlSerializer.toMeTLImage",() => {
		//println("deserializing image from xmpp: %s".format(input))
		val m = utils.parseMeTLContent(input)
		val c = utils.parseCanvasContent(input)
		val tag = utils.getStringByName(input,"tag")
		val source = utils.getStringByName(input,"source") match {
			case s:String if (s.length > 0 && s != "unknown url" && s != "none") => metlUtils.reabsolutizeUri(s,"Resource") 
			case _ => Empty
		}
		val imageBytes = source.map(u => {
			if (cacheImages)
				getCachedImage(u)
			else
				config.getResource(u)
		})
		val pngBytes = {
			if (transcodePng)
				imageBytes.map(b => {
					val inMs = new ByteArrayInputStream(b)
					val anyFormat = ImageIO.read(inMs)
					val out = new ByteArrayOutputStream
					ImageIO.write(anyFormat,"png",out)
					out.toByteArray
				})
			else Empty
		}
		val width = utils.getDoubleByName(input,"width")
		val height = utils.getDoubleByName(input,"height")
		val x = utils.getDoubleByName(input,"x")
		val y = utils.getDoubleByName(input,"y")
		MeTLImage(config,m.author,m.timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,c.target,c.privacy,c.slide,c.identity)
	})
	override def toMeTLCommand(input:NodeSeq):MeTLCommand = Stopwatch.time("MeTL2011XmlSerializer.toMeTLCommand", () => {
		val m = utils.parseMeTLContent(input)
		val body = input match {
			case t:Text => t.toString.split(" ")
			case e:Elem => utils.getStringByName(e,"body").split(" ")
			case other => other.toString.split(" ")
		}
		val comm = body.head
		val parameters = body.tail.toList
		MeTLCommand(config,m.author,m.timestamp,comm,parameters) 
	})
	override def fromMeTLCommand(input:MeTLCommand):NodeSeq = Stopwatch.time("MeTL2011XmlSerializer.fromMeTLCommand", () => {
		<body>{Text((input.command :: input.commandParameters).mkString(" "))}</body>
	})
	override def toMeTLQuiz(input:NodeSeq):MeTLQuiz = Stopwatch.time("MeTL2011XmlSerializer.toMeTLQuiz", () => {
		val m = utils.parseMeTLContent(input)
		val created = utils.getLongByName(input,"created")
		val question = utils.getStringByName(input,"question") match {
			case q if (q.length > 0) => q
			case _ => utils.getStringByName(input,"title")
		}
		val id = utils.getStringByName(input,"id")
		val url = utils.getStringByName(input,"url") match {
			case s:String if (s.length > 0 && s != "unknown url" && s != "none") => metlUtils.reabsolutizeUri(s,"Resource")
			case _ => Empty
		}
		val quizImage = url.map(u => {
			if (cacheImages)
				getCachedImage(u)
			else 
				config.getResource(u)
		})
		val isDeleted = utils.getBooleanByName(input,"isDeleted")
		val options = utils.getXmlByName(input,"quizOption").map(qo => toQuizOption(qo)).toList
		MeTLQuiz(config,m.author,m.timestamp,created,question,id,url,quizImage,isDeleted,options)	
	})
}
