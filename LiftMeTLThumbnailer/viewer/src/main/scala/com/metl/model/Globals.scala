package com.metl.model

import com.metl.data._
import com.metl.utils._
import com.metl.metl2011._

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
	def initializeWebMeTLSystem = {
		MeTL2011ServerConfiguration.initialize
		ServerConfiguration.loadServerConfigsFromFile("servers.monash.xml")
		ServerConfiguration.getServerConfigurations.foreach(c => LiftRules.unloadHooks.append(c.shutdown _))
	}
	def configForName(name:String):ServerConfiguration = ServerConfiguration.configForName(name)
	def configForHost(host:String):ServerConfiguration = ServerConfiguration.configForHost(host)
	def default:ServerConfiguration = {
		val host = tryo(xml.XML.loadString(Http.getClient.getAsString("http://metl.adm.monash.edu/server.xml")).text).openOr(ServerConfiguration.default.host)
		configForHost(host)
	}
}
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

