package com.metl.model

import scala.xml._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import Privacy._

import javax.imageio._
import java.io.{ByteArrayInputStream,ByteArrayOutputStream}

class MeTL2011XmlSerializer(configName:String) extends GenericXmlSerializer(configName){

	private val metlUtils = new MeTL2011Utils(configName)
	override def toMeTLImage(input:NodeSeq):MeTLImage = Stopwatch.time("MeTL2011XmlSerializer.toMeTLImage",() => {
		val m = utils.parseMeTLContent(input)
		val c = utils.parseCanvasContent(input)
		val tag = utils.getStringByName(input,"tag")
		val source = utils.getStringByName(input,"source") match {
			case s:String if (s.length > 0 && s != "unknown url" && s != "none") => metlUtils.reabsolutizeUri(s,"Resource") 
			case _ => Empty
		}
		val imageBytes = source.map(u => config.getResource(u))
		val pngBytes = imageBytes.map(b => {
			val inMs = new ByteArrayInputStream(b)
			val anyFormat = ImageIO.read(inMs)
			val out = new ByteArrayOutputStream
			ImageIO.write(anyFormat,"png",out)
			out.toByteArray
		})
		val width = utils.getDoubleByName(input,"width")
		val height = utils.getDoubleByName(input,"height")
		val x = utils.getDoubleByName(input,"x")
		val y = utils.getDoubleByName(input,"y")
		MeTLImage(config,m.author,m.timestamp,tag,source,imageBytes,pngBytes,width,height,x,y,c.target,c.privacy,c.slide,c.identity)
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
		val quizImage = url.map(u => config.getResource(u))
		val isDeleted = utils.getBooleanByName(input,"isDeleted")
		val options = utils.getXmlByName(input,"quizOption").map(qo => toQuizOption(qo)).toList
		MeTLQuiz(config,m.author,m.timestamp,created,question,id,url,quizImage,isDeleted,options)	
	})
}
