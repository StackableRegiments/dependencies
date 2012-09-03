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

object WebMeTLServerConfiguration {
	val empty = ServerConfiguration.empty
	val serverConfigs = Map(List(
		new MeTL2011BackendAdaptor("reifier","reifier.adm.monash.edu.au","http://meggle-prod.adm.monash.edu.au:8080/"),
		new MeTL2011BackendAdaptor("deified","deified.adm.monash.edu.au","http://meggle-prod.adm.monash.edu.au:8080/"),
		new MeTL2011BackendAdaptor("civic","civic.adm.monash.edu.au","http://meggle-ext.adm.monash.edu.au:8080/"),
		new MeTL2011BackendAdaptor("madam","madam.adm.monash.edu.au","http://meggle-staging.adm.monash.edu.au:8080/")
	).map(c => (c.name,c)):_*)
	def initializeWebMeTLSystem = {
		ServerConfiguration.setServerConfigurations(serverConfigs.values.toList)
		ServerConfiguration.setDefaultServerConfiguration(() => ServerConfiguration.configForHost(tryo(xml.XML.loadString(Http.getClient.getAsString("http://metl.adm.monash.edu/server.xml")).text).openOr("reifier.adm.monash.edu.au")))
	}
	def configForName(name:String):ServerConfiguration = serverConfigs.values.find(c => c.name == name).getOrElse(empty)
	def configForHost(host:String):ServerConfiguration = serverConfigs.values.find(c => c.host == host).getOrElse(empty)

	def default:ServerConfiguration ={
		val host = tryo(xml.XML.loadString(Http.getClient.getAsString("http://metl.adm.monash.edu/server.xml")).text).openOr("reifier.adm.monash.edu.au")
		configForHost(host)
	}
}
// I'll need to find out which of my packages are shadowing stuff
case class SnapshotResolution(width:Int,height:Int)

object SnapshotSize extends Enumeration {
	type SnapshotSize = Value
	val Thumbnail = Value

	def parse(name:String) ={
		name match {
			case _ => Thumbnail
		}
	}
}

object Globals {
	var isDevMode:Boolean = false

	val thumbnailSize = SnapshotResolution(320,240) // MeTL thumbnail

	val snapshotSizes = Map(
		SnapshotSize.Thumbnail -> thumbnailSize  // MeTL thumbnail
	)
}

