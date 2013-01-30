package com.metl.view

import com.metl.data._
import com.metl.utils._
import com.metl.renderer.SlideRenderer

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.http.provider._
import Helpers._
import com.metl.model._
import Http._
import scala.xml.XML
import org.apache.commons.io._

object MeTLRestHelper extends RestHelper {
  println("MeTLRestHelper inline")
  val serializer = new GenericXmlSerializer("rest")
  val crossDomainPolicy = {
    <cross-domain-policy>
    <allow-access-from domain="*" />
    </cross-domain-policy>
  }
  serve {
    case Req("serverStatus" :: Nil,_,_) =>
      () => Stopwatch.time("MeTLRestHelper.serverStatus", () => Full(PlainTextResponse("OK", List.empty[Tuple2[String,String]], 200)))
    case Req(List("crossdomain"),"xml",_) =>
      () => Stopwatch.time("MeTLRestHelper.crossDomainPolicy", () => Full(XmlResponse(crossDomainPolicy,200)))
    case r @ Req(List("summaries"),_,_) =>
      () => Stopwatch.time("MeTLRestHelper.summaries", () => StatelessHtml.summaries(r))
    case r @ Req(List("appcache"),_,_) =>
      () => Stopwatch.time("MeTLRestHelper.appcache", () => StatelessHtml.appCache(r))
    case r @ Req(List("history"),_,_) =>
      () => Stopwatch.time("MeTLRestHelper.history", () => StatelessHtml.history(r))
    case r @ Req(List("mergedHistory"),_,_) =>
      () => Stopwatch.time("MeTLRestHelper.mergedHistory", () => StatelessHtml.mergedHistory(r))
    case r @ Req(List("fullHistory"),_,_) =>
      () => Stopwatch.time("MeTLRestHelper.fullHistory", () => StatelessHtml.fullHistory(r))
    case r @ Req(List("details",jid),_,_) =>
      () => Stopwatch.time("MeTLRestHelper.details", () => StatelessHtml.details(jid))
    case Req("search" :: Nil,_,_) =>
      () => Stopwatch.time("MeTLStatefulRestHelper.search",() => {
        val query = S.params("query").head
        val server = ServerConfiguration.default
        Full(XmlResponse(<conversations>{server.searchForConversation(query).map(c => serializer.fromConversation(c))}</conversations>))
      })
    case Req("render" :: configName :: jid :: height :: width :: Nil,_,_) => Stopwatch.time("MeTLRestHelper.render", () => {
      val server = ServerConfiguration.configForName(configName)
      val history = MeTLXConfiguration.getRoom(jid,server.name).getHistory
      val image = SlideRenderer.render(history,width.toInt,height.toInt)
      Full(InMemoryResponse(image,List("Content-Type" -> "image/jpeg"),Nil,200))
    })
    case Req("thumbnail" :: configName :: jid :: Nil,_,_) => Stopwatch.time("MeTLRestHelper.thumbnail", () => {
      val server = ServerConfiguration.configForName(configName)
      val image = MeTLXConfiguration.getRoom(jid,server.name).getThumbnail
      Full(InMemoryResponse(image,List("Content-Type" -> "image/jpeg"),Nil,200))
    })
	}
}
object WebMeTLRestHelper extends RestHelper {
  println("WebMeTLRestHelper inline")
	serve {
		case Req("application" :: "snapshot" :: Nil,_,_) => () => {
			val server = S.param("server").openOr("")
			val slide = S.param("slide").openOr("")
			Full(HttpResponder.snapshot(server,slide,"small"))
		}
  }
}
object MeTLStatefulRestHelper extends RestHelper {
  println("MeTLStatefulRestHelper inline")
  val serializer = new GenericXmlSerializer("rest")
  serve {
    case Req(List("proxyDataUri",slide,source),_,_) =>
      ()=> Stopwatch.time("MeTLStatefulRestHelper.proxyDataUri",() => StatelessHtml.proxyDataUri(slide,source))
    case Req(List("proxy",slide,source),_,_) =>
      () => Stopwatch.time("MeTLStatefulRestHelper.proxy",() => StatelessHtml.proxy(slide,source))
    case Req(List("quizProxy",conversation,identity),_,_) =>
      () => Stopwatch.time("MeTLStatefulRestHelper.quizProxy", () => StatelessHtml.quizProxy(conversation,identity))
    case Req(List("submissionProxy",conversation,author,identity),_,_) =>
      () => Stopwatch.time("MeTLStatefulRestHelper.submissionProxy", () => StatelessHtml.submissionProxy(conversation,author,identity))
    case r@Req("join" :: Nil, _, _) => {
      for {
        conversationJid <- r.param("conversation");
        slide <- r.param("slide");
        sess <- S.session
      } yield {
        val serverConfig = ServerConfiguration.default
        val c = serverConfig.detailsOfConversation(conversationJid)
        println("Forced to join conversation %s".format(conversationJid))
        CurrentConversation(Full(c))
        if (c.slides.exists(s => slide.toLowerCase.trim == s.id.toString.toLowerCase.trim)){
          println("Forced move to slide %s".format(slide))
          CurrentSlide(Full(slide))
        }
      }
      RedirectResponse("/board")
    }
    case r @ Req("projector" :: conversationJid :: Nil, _, _) => {
      S.session match {
        case Full(sess) => {
          val serverConfig = ServerConfiguration.default
          val c = serverConfig.detailsOfConversation(conversationJid)
          if ((c.subject.toLowerCase.trim == "unrestricted" || Globals.getUserGroups.exists((ug:Tuple2[String,String]) => ug._2.toLowerCase.trim == c.subject.toLowerCase.trim)) && c != Conversation.empty){
            IsInteractiveUser(Full(false))
            CurrentConversation(Full(c))
            c.slides.sortBy(s => s.index).headOption.map(s => CurrentSlide(Full(s.id.toString)))
          }
        }
        case _ => {}
      }
      RedirectResponse("/board")
    }
    case r @ Req(List("upload"),_,_) =>{
      println("Upload registered in MeTLStatefulRestHelper")
      println(r.body)
      () => Stopwatch.time("MeTLStatefulRestHelper.upload",() => {
        r.body.map(bytes => {
          val filename = S.params("filename").head
          val jid = S.params("jid").head
          val server = ServerConfiguration.default
          XmlResponse(<resourceUrl>{server.postResource(jid,filename,bytes)}</resourceUrl>)
        })
      })
    }
    case r @ Req(List("uploadDataUri"),_,_) =>{
      println("UploadDataUri registered in MeTLStatefulRestHelper")
      println(r.body)
      () => Stopwatch.time("MeTLStatefulRestHelper.upload",() => {
        r.body.map(dataUriBytes => {
          val dataUriString = IOUtils.toString(dataUriBytes)
          val b64Bytes = dataUriString.split(",")(1)
          val bytes = net.liftweb.util.SecurityHelpers.base64Decode(b64Bytes)
          val filename = S.params("filename").head
          val jid = S.params("jid").head
          val server = ServerConfiguration.default
          XmlResponse(<resourceUrl>{server.postResource(jid,filename,bytes)}</resourceUrl>)
        })
      })
    }
    case r @ Req(List("logDevice"),_,_) => () => {
      r.userAgent.map(ua => {
        println("UserAgent:"+ua)
        PlainTextResponse("loggedUserAgent")
      })
    }
	}
}
object WebMeTLStatefulRestHelper extends RestHelper {
  println("WebMeTLStatefulRestHelper inline")
  serve {
		case Req(server :: "slide" :: jid :: size :: Nil,_,_) => () => Full(HttpResponder.snapshot(server,jid,size))
		case Req(server :: "quizImage" :: jid :: id :: Nil,_,_) => () => Full(HttpResponder.quizImage(server,jid,id))
		case Req(server :: "quizResponse" :: conversation :: quiz :: response :: Nil,_,_)
			if (List(server,conversation,quiz,response).filter(_.length == 0).isEmpty) => () => {
				val slide = S.param("slide").openOr("")
				Full(QuizResponder.handleResponse(server,conversation,slide,quiz,response))
			}
  }
}
