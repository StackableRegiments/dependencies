package com.metl.metl2011

import com.metl.data._
import net.liftweb.common.Logger
import net.liftweb.util._
import org.apache.commons.io.IOUtils

import scala.xml._

class MeTL2011Resources(configName:String, http:HttpProvider) extends Logger {
	lazy val config = ServerConfiguration.configForName(configName)
	lazy val utils = new MeTL2011Utils(configName)
	lazy val rootAddress = "https://%s:1188".format(config.host)
	def postResource(jid:String,userGeneratedId:String,data:Array[Byte]):String = {
		val uri = "%s/upload_nested.yaws?path=%s&overwrite=false&filename=%s".format(rootAddress,Helpers.urlEncode("Resource/%s/%s".format(utils.stem(jid.toString),jid.toString)),Helpers.urlEncode(userGeneratedId))	
		val response = http.getClient.postBytes(uri,data)
		val responseString = IOUtils.toString(response)
		debug("postedResource response: %s".format(responseString))
		((XML.loadString(responseString) \\ "resource").head \ "@url").text
	}
}
