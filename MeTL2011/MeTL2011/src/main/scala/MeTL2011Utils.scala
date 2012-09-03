package com.metl.metl2011

import com.metl.data._
import com.metl.utils._

import net.liftweb._
import http._
import common._
import util._
import Helpers._
import collection._

class MeTL2011Utils(configName:String) {
	lazy val config = ServerConfiguration.configForName(configName)
	def stem(path:String):String = (("0"*(5 - path.length)) + path).takeRight(5).take(2).mkString
	def reabsolutizeUri(uri:String,prefix:String):Box[String]={
		val path = new java.net.URI(uri).getPath
		val pathparts = path.split("/").filter(_ != "").toList 
		def construct(stemmed:String):Box[String] = Full("https://%s:1188/%s".format(config.host,stemmed))
		pathparts match{
			case List(prefix,stemmed,stemmable,_*) if (stem(stemmable) == stemmed) => construct((List(prefix,stemmed,stemmable) ::: pathparts.drop(3)).mkString("/"))
			case List(prefix,unstemmed,_*)=> construct((List(prefix,stem(unstemmed),unstemmed) ::: pathparts.drop(2)).mkString("/"))
			case List(noPrefix)=> construct(noPrefix)
			case Nil => Empty
		}
	}
}
