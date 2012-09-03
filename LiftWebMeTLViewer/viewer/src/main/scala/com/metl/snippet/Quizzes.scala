package com.metl.snippet

import com.metl.model._
import com.metl.snippet.Utils._
import scala.xml._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import Helpers._
import S._

class QuizzesSnippet {
	object server extends RequestVar[WebMeTLServerConfiguration](Utils.prepareServerFromRequest)
	object conversationId extends RequestVar[String](S.param("conversation").openOr(""))
	object slideId extends RequestVar[String](S.param("slide").openOr(""))

	def title(in:NodeSeq):NodeSeq = conversationId.is match {
		case s:String if (s.length > 0) => {
			ConversationCache.getConversation(conversationId.is.toInt,server.serverConfig) match {
				case d:Conversation if (d != Conversation.empty) => Text(d.title)
				case _ => NodeSeq.Empty
			}
		}
		case _ => NodeSeq.Empty
	}

	def navigation:NodeSeq = (conversationId) match {
		case (cid) if (cid.length>0) => {
			Utils.navLinks(List(
				Link("slidesLink","/slide?server=%s&conversation=%s&slide=%s".format(server.is.name,cid,slideId.is),"View slides"),
				Link("quizzesLink","/quizzes?server=%s&conversation=%s&slide=%s".format(server.is.name,cid,slideId.is),"Quizzes")
			))
		}
        case _ => NodeSeq.Empty
	}

	def list = handleParamErrors.openOr(renderList)

	private def renderList = "#quizList *" #> {
		HistoryCache.getHistory(conversationId.is,server.is.serverConfig).getQuizzes.sortBy(q => q.id).map(quiz => {
			<div class="quizItem">
				<a href={"/quiz?server=%s&conversation=%s&slide=%s&quiz=%s".format(server.is.name,conversationId.is,slideId.is,quiz.id)}>
					<span>{quiz.question}</span>
				</a>
			</div>
		})
	}
	
	private def renderListError(message:String) = "#quizListError *" #> Text(message)
	private def handleParamErrors:Box[CssSel] ={
		(conversationId.is) match {
			case (cid) if (cid.length>0) => {
				HistoryCache.getHistory(cid,server.is.serverConfig) match {
					case h:History if (h == History.empty) => Full(renderListError("couldn't get conversation history"))
					case h:History => Empty
				}
			}
			case _ => Full(renderListError("need conversation param"))
		}
	}
}
