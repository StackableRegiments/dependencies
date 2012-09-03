package com.metl.snippet

import com.metl.model._
import com.metl.snippet.Utils._
import scala.xml._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.http.js._
import Helpers._
import S._
import SHtml._
import JsCmds._
import java.util.Date

object QuizTemplates {
	def quizOption = Templates(List("_quizOption")).openOr(NodeSeq.Empty)
}

class QuizSnippet {
	object server extends RequestVar[WebMeTLServerConfiguration](Utils.prepareServerFromRequest)
	object conversationId extends RequestVar[String](S.param("conversation").openOr(""))
	object slideId extends RequestVar[String](S.param("slide").openOr(""))
	object quizId extends RequestVar[String](S.param("quiz").openOr(""))

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

	def previous(in:NodeSeq):NodeSeq = getNearbyQuizId(-1).map(previousId => renderPrevious(previousId).apply(in)).openOr(in)
	def next(in:NodeSeq):NodeSeq = getNearbyQuizId(1).map(nextId => renderNext(nextId).apply(in)).openOr(in)

	private def renderPrevious(previousId:String) =
		"#quizNavigationPrevious *" #> <a href={"/quiz?server=%s&conversation=%s&slide=%s&quiz=%s".format(server.is.name,conversationId.is,slideId.is,previousId)}><span>Previous quiz</span></a>

	private def renderNext(nextId:String) =
		"#quizNavigationNext *" #> <a href={"/quiz?server=%s&conversation=%s&slide=%s&quiz=%s".format(server.is.name,conversationId.is,slideId.is,nextId)}><span>Next quiz</span></a>
	
	private def getNearbyQuizId(offset:Int):Box[String] = (conversationId.is,quizId.is) match {
		case (cid,qid) if (cid.length>0 && qid.length>0) => {
			HistoryCache.getHistory(conversationId.is,server.is.serverConfig) match {
				case h:History if (h != History.empty) => {
					val quizzes = h.getQuizzes.sortBy(q => q.id)
					quizzes.length match {
						case 0 => Empty
						case 1 => Full(qid)
						case _ => quizzes.indexWhere(q => q.id == quizId.is) match {
							case currentIndex:Int if (currentIndex >= 0) => {
								val wantedIndex = (currentIndex+offset) match {
									case i:Int if i >= quizzes.length => i % quizzes.length
									case i:Int if i < 0 => quizzes.length + (i % quizzes.length)
									case i:Int => i
								}
								Full(quizzes(wantedIndex).id)
							}
							case _ => Empty
						}
					}
				}
				case _ => Empty
			}
		}
		case _ => Empty
	}

	def quiz(in:NodeSeq):NodeSeq = handleParamErrors.openOr(renderQuiz(in)).apply(in)

	private def renderQuiz(in:NodeSeq) = {
		val history = HistoryCache.getHistory(conversationId.is,server.is.serverConfig)
		history.getQuizzes.find(quiz => quiz.id == quizId.is).map(quiz => {
			val responses = history.getQuizResponses.filter(qr => qr.id == quiz.id)
			val userAnswer = responses.filter(qr => qr.answerer == Globals.currentUser.is).sortBy(qr => qr.timestamp).reverse.headOption
			"#quizQuestion *" #> quiz.question &
			"#quizOptions *" #> {
				quiz.options.map(option => ResponseItem(option.name,option.text,option.color,userAnswer.map(qr => qr.answer == option.name).getOrElse(false))).foldLeft(NodeSeq.Empty)((acc,item) => acc ++ renderResponseItem(item).apply(QuizTemplates.quizOption))
			} &
			"#quizImage *" #> quiz.url.map(qu => <img src={"/%s/quizImage/%s/%s".format(server.is.name,conversationId.is,quiz.id)} />).openOr(NodeSeq.Empty)
		}).getOrElse(ClearClearable)
	}

	case class ResponseItem(name:String,text:String,color:Color,isUserResponse:Boolean)
	private def renderResponseItem(item:ResponseItem) =
		".quizOption [class+]" #> { item.isUserResponse match {
			case true  => "quizUserResponse"
			case false => "quizNotUserResponse"
		} } &
		".quizOptionName *" #> item.name &
		".quizOptionName [style]" #> "background-color: %s".format(ColorConverter.toHexString(item.color)) &
		".quizOptionText *" #> item.text &
		".quizOptionAnchor [href]" #> "/%s/quizResponse/%s/%s/%s?slide=%s".format(server.is.name,conversationId.is,quizId.is,item.name,slideId.is)

	private def renderQuizError(message:String) = "#quizError *" #> Text(message)
	private def handleParamErrors:Box[CssSel] ={
		(conversationId.is,quizId.is) match {
			case (cid,qid) if (cid.length>0 && qid.length>0) => {
				HistoryCache.getHistory(cid,server.is.serverConfig) match {
					case h:History if (h == History.empty) => Full(renderQuizError("couldn't get conversation history"))
					case h:History => h.getQuizzes.find(q => q.id == qid) match {
						case Some(q) => Empty
						case _ => Full(renderQuizError("couldn't find quiz"))
					}
				}
			}
			case _ => Full(renderQuizError("need conversation and quiz param"))
		}
	}
}
