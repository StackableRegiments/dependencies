package com.metl.model

import net.liftweb.util._
import org.apache.commons.io.IOUtils

class MeTL2011Conversations(configName:String, val searchBaseUrl:String, http:SimpleAuthedHttpProvider) extends ConversationRetriever(configName) {
	lazy val utils = new MeTL2011Utils(configName)
	lazy val serializer = new MeTL2011XmlSerializer(configName)
	lazy val rootAddress = "https://%s:1188".format(config.host)

	override def search(query:String):List[Conversation] = Stopwatch.time("Conversations.search", () => {
		(scala.xml.XML.loadString(http.getClient.get(searchBaseUrl + "search?query=" + Helpers.urlEncode(query))) \\ "conversation").map(c => serializer.toConversation(c)).toList
	})
	override def conversationFor(slide:Int):Int = Stopwatch.time("Conversations.conversationFor",() => {
		config.name match {
			case "reifier" => ((slide / 1000) * 1000) + 400
			case "deified" => ((slide / 1000) * 1000) + 400
			case _ => (slide /1000) * 1000
		}
	})
	override def detailsOf(jid:Int):Conversation = Stopwatch.time("Conversations.detailsOf",() => {
		(scala.xml.XML.loadString(http.getClient.get("https://"+config.host+":1188/Structure/"+utils.stem(jid.toString)+"/"+jid.toString+"/details.xml")) \\ "conversation").headOption.map(c => serializer.toConversation(c)).getOrElse(Conversation.empty)
	})
	override def createConversation(title:String):Conversation = {
		val currentUser = "RogerTest"
		val jid = getNewJid
		val now = new java.util.Date()
		val local = Conversation(config,currentUser,now.getTime,List(Slide(config,currentUser,jid + 1,0)),"unrestricted","",jid,title,now.toString,Permissions.default(config))	
		val pbytes = serializer.fromConversation(local).toString.getBytes("UTF-8")
		http.getClient.postBytes("%s/upload_nested.yaws?overwrite=true&path=%s&filename=details.xml".format(rootAddress,Helpers.urlEncode("Structure/%s/%s".format(utils.stem(jid.toString),jid.toString))),pbytes)
		detailsOf(jid)
	}
	private def notifyXmpp(newConversation:Conversation) = {
		// need to write this, obviously
	}
	override def updateConversation(jid:Int,conversation:Conversation) = conversation
	private def getNewJid:Int = http.getClient.get("https://"+config.host+":1188/primarykey.yaws").trim.toInt
}
