package com.metl.embedded

import com.metl.data._
import com.metl.utils._

import net.liftweb.common._
import java.util.Date
import net.liftweb.util._
import scala.collection.JavaConversions._
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonParser._
import net.liftweb.http.js.JE._
import java.net.URL
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import org.apache.commons.io.IOUtils
import collection.JavaConversions._
import xml._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.json.Serialization

import java.io.{InputStream,OutputStream}

class LocalEmbeddedInterface(host:String,port:Int,db:String) {
	def storeStanza(jid:String,stanza:MeTLStanza):Boolean = {
		true
	}
}

