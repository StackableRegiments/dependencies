package com.metl.embedded

import com.metl.data._
import com.metl.utils._

import net.liftweb.common._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonParser._
import net.liftweb.http.js.JE._
import java.util.Date
import scala.collection._
import scala.collection.JavaConversions._
import scala.io._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._

class EmbeddedResourceProvider(configName:String){
	def getResource(identity:String):Array[Byte] = {
		Array.empty[Byte]
	}
	def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = {
		""
	}
}
