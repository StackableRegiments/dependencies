package com.metl.model

import com.metl.utils._
import com.metl.data._

import net.liftweb.actor.LiftActor
import net.liftweb.util.Helpers
import net.liftweb.common._

case class UserActivityLogRecord(who:String,url:String) {
	private val queryFormat = "who=%s&url=%s"
	def makeQueryFragment = queryFormat.format(Helpers.urlEncode(who),Helpers.urlEncode(url))
}

object EmptyLogActor extends LogActor("empty") {
	override lazy val http:Box[CleanHttpClient] = Empty
}

class LogActor(host:String) extends LiftActor {
	private val program = "metlviewer"

	protected lazy val http:Box[CleanHttpClient] = Full(Http.getClient)

	private val logUrlFormat = "https://%s:1188/log_message.yaws?program=%s&%s"
	private def makeUrl(queryFragment:String) = logUrlFormat.format(host,program,queryFragment)

	private def saveUserActivityLogRecord(rec:UserActivityLogRecord) = http.map(h => h.get(makeUrl(rec.makeQueryFragment)))

	override def messageHandler ={
		case rec:UserActivityLogRecord => saveUserActivityLogRecord(rec)
		case _ => println("LogActor(%s) received unknown message".format(host))
	}
}
