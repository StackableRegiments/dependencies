package com.metl.orientDB.dbformats

import javax.persistence.{Version, Id}

import com.metl.data._
import com.metl.utils._

import net.liftweb.common._
import net.liftweb.util.Helpers._

class OPrivacy {
	var p:String = _
	override def toString = "Privacy(%s)".format(p)
}

class OColor {
	var r:java.lang.Integer = _
	var g:java.lang.Integer = _
	var b:java.lang.Integer = _
	var a:java.lang.Integer = _
	override def toString = "Color(%s,%s,%s,%s)".format(r,g,b,a)
}

class OPoint {
	var x:Double = _
	var y:Double = _
	var pressure:java.lang.Integer = _
	override def toString = "Point(%s,%s,%s)".format(x,y,pressure)
}

class OInk {
	@Id var id: String = _
	@Version var version: java.lang.Integer = _
	var author: String = _
	var timestamp : Long = _
	var checksum: Double = _
	var startingSum: Double = _
	var points:java.util.List[OPoint] = new java.util.ArrayList()
	var color:OColor = _
	var thickness:Double = _
	var isHighlighter:Boolean = _
	var target:String = _
	var privacy:OPrivacy = _
	var slide:String = _
	var identity:String = _
	override def toString = "Ink(%s,%s,%s,%s,%s)".format(identity,author,privacy,slide,target)
}

class OImage {
	@Id var id: String = _
	@Version var version: java.lang.Integer = _
	var author: String = _
	var timestamp : Long = _
	var source: String = _
	var height: Double = _
	var width: Double = _
	var x:Double = _
	var y:Double = _
	var target:String = _
	var privacy:OPrivacy = _
	var slide:String = _
	var identity:String = _
	override def toString = "Image(%s,%s,%s,%s,%s,%s)".format(identity,source,author,privacy,slide,target)
}

class OText {
	@Id var id: String = _
	@Version var version: java.lang.Integer = _
	var author: String = _
	var timestamp : Long = _
	var text:String = _
	var caret:java.lang.Integer = _
	var tag:String = _
	var style:String = _
	var family:String = _
	var weight:String = _
	var size:Double = _
	var decoration:String = _
	var color:OColor = _
	var height: Double = _
	var width: Double = _
	var x:Double = _
	var y:Double = _
	var target:String = _
	var privacy:OPrivacy = _
	var slide:String = _
	var identity:String = _
	override def toString = "Text(%s,%s,%s,%s,%s,%s)".format(identity,text,author,privacy,slide,target)
}

class OMoveDelta {
	@Id var id: String = _
	@Version var version: java.lang.Integer = _
	var author: String = _
	var timestamp : Long = _
	var inkIds:java.util.List[String] = new java.util.ArrayList()
	var textIds:java.util.List[String] = new java.util.ArrayList()
	var imageIds:java.util.List[String] = new java.util.ArrayList()
	var xTranslate:Double = _
	var yTranslate:Double = _
	var xScale:Double = _
	var yScale:Double = _
	var newPrivacy:OPrivacy = _
	var isDeleted:Boolean = _
	var target:String = _
	var privacy:OPrivacy = _
	var slide:String = _
	var identity:String = _
	override def toString = "MoveDelta(%s,%s,%s,%s,%s)".format(identity,author,privacy,slide,target)
}

class ODirtyInk {
	@Id var id: String = _
	@Version var version: java.lang.Integer = _
	var author: String = _
	var timestamp : Long = _
	var target:String = _
	var privacy:OPrivacy = _
	var slide:String = _
	var identity:String = _
	override def toString = "DirtyInk(%s,%s,%s,%s,%s)".format(identity,author,privacy,slide,target)
}

class ODirtyImage {
	@Id var id: String = _
	@Version var version: java.lang.Integer = _
	var author: String = _
	var timestamp : Long = _
	var target:String = _
	var privacy:OPrivacy = _
	var slide:String = _
	var identity:String = _
	override def toString = "DirtyImage(%s,%s,%s,%s,%s)".format(identity,author,privacy,slide,target)
}

class ODirtyText {
	@Id var id: String = _
	@Version var version: java.lang.Integer = _
	var author: String = _
	var timestamp : Long = _
	var target:String = _
	var privacy:OPrivacy = _
	var slide:String = _
	var identity:String = _
	override def toString = "DirtyText(%s,%s,%s,%s,%s)".format(identity,author,privacy,slide,target)
}

class OCommand {
	@Id var id:String = _
	@Version var version: java.lang.Integer = _
	var author:String = _
	var timestamp:Long = _
	var command:String = _
	var commandParameters:java.util.List[String] = new java.util.ArrayList()
	override def toString = "Command(%s,%s,%s)".format(command,author,commandParameters.toString)
}

class OSubmission {
	@Id var id:String = _
	@Version var version: java.lang.Integer = _
	var author:String = _
	var timestamp:Long = _
	var title:String = _
	var slideJid:java.lang.Integer = _
	var url:String = _
	var blacklist:java.util.List[OBlacklistItem] = new java.util.ArrayList()
	var target:String = _
	var privacy:OPrivacy = _
	var identity:String = _
	override def toString = "Submission(%s,%s,%s,%s)".format(author,url,timestamp,identity)

}

class OBlacklistItem {
	var username:String = _
	var highlight:OColor = _
	override def toString = "BlacklistItem(%s,%s)".format(username,highlight)
}

class OQuiz {
	@Id var id:String = _
	@Version var version: java.lang.Integer = _
	var author:String = _
	var timestamp:Long = _
	var created:Long = _
	var question:String = _
	var quizId:String = _
	var url:String = _
	var isDeleted:Boolean = _
	var options:java.util.List[OQuizOption] = new java.util.ArrayList()
	override def toString = "Comamand(%s,%s,%s)".format(id,author,isDeleted)
}

class OQuizOption {
	var name:String = _
	var text:String = _
	var correct:Boolean = _
	var color:OColor = _
	override def toString = "QuizOption(%s,%s,%s)".format(name,text,color)
}

class OQuizResponse {
	@Id var id:String = _
	@Version var version: java.lang.Integer = _
	var author:String = _
	var timestamp:Long = _
	var answer:String = _
	var answerer:String = _
	var quizId:String = _
	override def toString = "QuizResponse(%s,%s,%s,%s)".format(answerer,answer,id,timestamp)
}

class OResource {
	@Id var id: String = _
	@Version var version: java.lang.Integer = _
	var jid: String = _
	var identifier: String = _
	var bytes: Array[Byte] = _
	override def toString = "Resource(Id(%s),Jid(%s),Identifier(%s),Version(%s))".format(id,jid,identifier,version)
}
