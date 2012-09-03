package com.metl.model

import net.liftweb.common._
import net.liftweb.http._
import scala.xml._
import java.util.Date

object QuizResponder {

	def handleResponse(serverName:String,conversationId:String,slideId:String,quizId:String,response:String) ={
		val wmConfig = WebMeTLServerConfiguration.configForName(serverName)
		val server = wmConfig.serverConfig
		val qr = MeTLQuizResponse(server,Globals.currentUser.is,new Date().getTime,response,Globals.currentUser.is,quizId)
		val qrm = QuizResponseMessage(conversationId,qr)
		wmConfig.quizResponseActor ! qrm
		HistoryCache.addQuizResponseToHistory(conversationId,server,qr)
		RedirectResponse("/quiz?server=%s&conversation=%s&slide=%s&quiz=%s".format(server.name,conversationId,slideId,quizId))
	}

}
