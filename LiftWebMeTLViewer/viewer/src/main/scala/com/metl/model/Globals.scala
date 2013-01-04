package com.metl.model

import com.metl.data._
import com.metl.metl2011._
import com.metl.utils._
import com.metl.cas._

import scala.xml._
import _root_.net.liftweb.util._
import Helpers._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.rest._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._

case class WebMeTLServerConfiguration(name:String,serverConfig:ServerConfiguration,quizResponseActor:QuizResponseActor,logActor:LogActor){
	val host = serverConfig.host
}

object WebMeTLServerConfiguration {
	val auth = new com.metl.cas.CASAuthenticator("webmetl",() => Globals.casState.authenticated, (cs:com.metl.cas.CASStateData) => {
		println("loginHandler")
		Globals.casState(cs)
		Globals.currentUser(cs.username)
	})

	val empty = WebMeTLServerConfiguration("empty",ServerConfiguration.empty,EmptyQuizResponseActor,EmptyLogActor)

	MeTL2011ServerConfiguration.initialize
	ServerConfiguration.loadServerConfigsFromFile("servers.monash.xml")
	val servers = ServerConfiguration.getServerConfigurations
	val serverConfigs = Map(("empty",empty) :: servers.map(s => (s.name,WebMeTLServerConfiguration(s.name,s,new QuizResponseActor(s.host),new LogActor(s.host)))):_*)
	def initializeWebMeTLSystem = {
	  Props.mode match {
	    case Props.RunModes.Production => Globals.isDevMode = false
  	  case _=> Globals.isDevMode = true
    }
		Globals.isDevMode match {
			case false => {
    		println("initializing the WebMeTL System in production mode")
				com.metl.cas.CASAuthentication.attachCASAuthenticator(auth)
			}
			case _ => {
				println("initializing the WebMeTL System in development mode")
			}
		}
	}
	def configForName(name:String):WebMeTLServerConfiguration = serverConfigs.values.find(c => c.name == name).getOrElse(empty)
	def configForHost(host:String):WebMeTLServerConfiguration = serverConfigs.values.find(c => c.host == host).getOrElse(empty)

	def default:WebMeTLServerConfiguration ={
		val host = tryo(xml.XML.loadString(Http.getClient.getAsString("http://metl.adm.monash.edu/server.xml")).text).openOr(ServerConfiguration.default.host)
		configForHost(host)
	}
}
case class SnapshotResolution(width:Int,height:Int)

object SnapshotSize extends Enumeration {
	type SnapshotSize = Value
	val Thumbnail, Small, Medium, Large = Value

	def parse(name:String) ={
		name match {
			case "thumbnail" => Thumbnail
			case "small"  => Small
			case "medium" => Medium
			case "large"  => Large
			case _ => Medium
		}
	}
}

object Globals {
	var isDevMode:Boolean = false
  object casState extends SessionVar[com.metl.cas.CASStateData](com.metl.cas.CASStateDataForbidden)
  object currentUser extends SessionVar[String](casState.is.username)

	val thumbnailSize = SnapshotResolution(320,240) // MeTL thumbnail

	val snapshotSizes = Map(
		SnapshotSize.Thumbnail -> thumbnailSize,  // MeTL thumbnail
		SnapshotSize.Small  -> SnapshotResolution(640,480),  // MeTL small for phones
		SnapshotSize.Medium -> SnapshotResolution(1024,768), // dunno, seems like a good midpoint
		SnapshotSize.Large  -> SnapshotResolution(2560,1600) // WQXGA, largest reasonable size (we guess)
	)
}

