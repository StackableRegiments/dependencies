package com.metl.metl2011

import com.metl.data._
import com.metl.utils._

import net.liftweb.util._
import org.apache.commons.io.IOUtils

class MeTL2011Conversations(configName:String, val searchBaseUrl:String, http:SimpleAuthedHttpProvider,messageBusProvider:MessageBusProvider,onConversationDetailsUpdated:(Conversation) => Unit) extends ConversationRetriever(configName) {
	lazy val utils = new MeTL2011Utils(configName)
	lazy val serializer = new MeTL2011XmlSerializer(configName)
	lazy val rootAddress = "https://%s:1188".format(config.host)
	val mbDef = new MessageBusDefinition("global","conversationUpdating",receiveConversationDetailsUpdated _)
	val mb = messageBusProvider.getMessageBus(mbDef)
	println("created MeTL2011 Conversation Provider with MessageBus: %s (%s)".format(mb,mbDef))
	
	def receiveConversationDetailsUpdated(m:MeTLStanza) = {
		println("message on the global thread received: %s".format(m))
		m match {
			case c:MeTLCommand if c.command == "/UPDATE_CONVERSATION_DETAILS" && c.commandParameters.length == 1 => {
				try{
					println("metlCommand comprehended on the global thread: %s".format(c))
					onConversationDetailsUpdated(detailsOf(c.commandParameters(0).toInt))
				} catch {
					case e:Throwable => println("exception while attempting to update conversation details")
				}
			}
			case _ => {}
		}
	}

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
	override def createConversation(title:String,author:String):Conversation = {
		val jid = getNewJid
		val now = new java.util.Date()
		val local = Conversation(config,author,now.getTime,List(Slide(config,author,jid + 1,0)),"unrestricted","",jid,title,now.toString,Permissions.default(config))	
		pushConversationToServer(local)
	}
	override def deleteConversation(jid:String):Conversation = {
		val conv = detailsOf(jid.toInt)
		val now = new java.util.Date()
		val local = Conversation(config,conv.author,now.getTime,conv.slides,"deleted",conv.tag,conv.jid,conv.title,conv.created,conv.permissions)
		pushConversationToServer(local)
	}
	override def renameConversation(jid:String,newTitle:String):Conversation = {
		val conv = detailsOf(jid.toInt)
		val now = new java.util.Date()
		val local = Conversation(config,conv.author,now.getTime,conv.slides,conv.subject,conv.tag,conv.jid,newTitle,conv.created,conv.permissions)
		pushConversationToServer(local)
	}
	override def changePermissions(jid:String,newPermissions:Permissions):Conversation = {
		val conv = detailsOf(jid.toInt)
		val now = new java.util.Date()
		val local = Conversation(config,conv.author,now.getTime,conv.slides,conv.subject,conv.tag,conv.jid,conv.title,conv.created,newPermissions)
		pushConversationToServer(local)
	}
	override def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = {
		val conv = detailsOf(jid.toInt)
		val now = new java.util.Date()
		val local = Conversation(config,conv.author,now.getTime,conv.slides,newSubject,conv.tag,conv.jid,conv.title,conv.created,conv.permissions)
		pushConversationToServer(local)
	}
	override def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = {
		val conv = detailsOf(jid.toInt)
		val slides = conv.slides
		val currentMaxJid = slides.map(s => s.id).max
		val newSlides = slides.map(s => {
			val newIndex = s.index match {
				case i:Int if (i < index) => i
				case i:Int => i + 1
			}
			Slide(config,s.author,s.id,newIndex,s.defaultHeight,s.defaultWidth,s.exposed,s.slideType)
		})
		val newSlide = Slide(config,conv.author,currentMaxJid + 1, index)
		val now = new java.util.Date()
		val local = Conversation(config,conv.author,now.getTime,newSlide :: newSlides,conv.subject,conv.tag,conv.jid,conv.title,conv.created,conv.permissions)
		pushConversationToServer(local)
	}
	override def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = {
		val conv = detailsOf(jid.toInt)
		val now = new java.util.Date()
		val local = Conversation(config,conv.author,now.getTime,newSlides,conv.subject,conv.tag,conv.jid,conv.title,conv.created,conv.permissions)
		pushConversationToServer(local)
	}
	private def pushConversationToServer(conversation:Conversation):Conversation = {
		val jid = conversation.jid
		val bytes = serializer.fromConversation(conversation).toString.getBytes("UTF-8")
		http.getClient.postBytes("%s/upload_nested.yaws?overwrite=true&path=%s&filename=details.xml".format(rootAddress,Helpers.urlEncode("Structure/%s/%s".format(utils.stem(jid.toString),jid.toString))),bytes)
		val remote = detailsOf(jid)
		notifyXmpp(remote)
		remote
	}
	private def notifyXmpp(newConversation:Conversation) = {
		mb.sendStanzaToRoom(MeTLCommand(config,newConversation.author,new java.util.Date().getTime,"/UPDATE_CONVERSATION_DETAILS",List(newConversation.jid.toString)))
	}
	private def getNewJid:Int = http.getClient.get("https://"+config.host+":1188/primarykey.yaws").trim.toInt
}
