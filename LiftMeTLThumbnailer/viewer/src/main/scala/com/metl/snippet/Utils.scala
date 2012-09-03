package com.metl.snippet

import com.metl.model._
import scala.xml._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import Helpers._
import S._

object Utils {
	def navLinkTemplate = Templates(List("_navLink")).openOr(NodeSeq.Empty)

	case class Link(clazz:String,url:String,text:String)
	def navLinks(links:List[Link]):NodeSeq =
		links.foldLeft(NodeSeq.Empty)((acc,item) => acc ++ renderNavLink(item).apply(navLinkTemplate))
		
	private def renderNavLink(link:Link) =
		".navLinkAnchor [class+]" #> link.clazz &
		".navLinkAnchor [href]" #> link.url &
		".navLinkText *" #> Text(link.text)


	def prepareServerFromRequest ={
		val server = S.param("server").map(name => WebMeTLServerConfiguration.configForName(name)).openOr(WebMeTLServerConfiguration.default)
		println("preparedServerFromRequest: %s".format(server))
		server.logActor ! UserActivityLogRecord(Globals.currentUser.is,S.uriAndQueryString.openOr("[unknown]"))
		server
	}

}
