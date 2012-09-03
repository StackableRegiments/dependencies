package com.metl.model

import scala.xml._
import _root_.net.liftweb.util._
import Helpers._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.rest._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import com.metl.cas._

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
	val serverConfigs = Map(List(
		empty,
		WebMeTLServerConfiguration("reifier",new MeTL2011BackendAdaptor("reifier","reifier.adm.monash.edu.au","http://meggle-prod.adm.monash.edu.au:8080/"),new QuizResponseActor("reifier"),new LogActor("reifier.adm.monash.edu.au")),
		WebMeTLServerConfiguration("deified",new MeTL2011BackendAdaptor("deified","deified.adm.monash.edu.au","http://meggle-prod.adm.monash.edu.au:8080/"),new QuizResponseActor("deified"),new LogActor("deified.adm.monash.edu.au")),
		WebMeTLServerConfiguration("civic",new MeTL2011BackendAdaptor("civic","civic.adm.monash.edu.au","http://meggle-ext.adm.monash.edu.au:8080/"),new QuizResponseActor("civic"),new LogActor("civic.adm.monash.edu.au")),
		WebMeTLServerConfiguration("madam",new MeTL2011BackendAdaptor("madam","madam.adm.monash.edu.au","http://meggle-staging.adm.monash.edu.au:8080/"),new QuizResponseActor("madam"),new LogActor("madam.adm.monash.edu.au"))
	).map(c => (c.name,c)):_*)
	def initializeWebMeTLSystem = {
		ServerConfiguration.setServerConfigurations(serverConfigs.values.map(_.serverConfig).toList)
		ServerConfiguration.setDefaultServerConfiguration(() => ServerConfiguration.configForHost(tryo(xml.XML.loadString(Http.getClient.getAsString("http://metl.adm.monash.edu/server.xml")).text).openOr("reifier.adm.monash.edu.au")))
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
		val host = tryo(xml.XML.loadString(Http.getClient.getAsString("http://metl.adm.monash.edu/server.xml")).text).openOr("reifier.adm.monash.edu.au")
		configForHost(host)
	}
}
// I'll need to find out which of my packages are shadowing stuff
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

