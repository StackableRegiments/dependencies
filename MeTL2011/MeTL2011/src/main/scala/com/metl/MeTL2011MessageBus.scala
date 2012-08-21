package com.metl.model

import org.jivesoftware.smack._
import org.jivesoftware.smack.filter._
import org.jivesoftware.smack.packet._
import org.jivesoftware.smackx.muc._
import net.liftweb.actor._
import net.liftweb.common._
import java.util.Random
import net.liftweb.util.Helpers._
import scala.xml._
import java.util.Date

class XmppProvider(configName:String,hostname:String,username:String,password:String) extends MessageBusProvider(configName){
	override def getMessageBus(jid:String) = new XmppMessageBus(jid,configName,hostname,username,password)
}

class MeTL2011XmppConn(u:String,p:String,r:String,h:String,configName:String) extends XmppConnection[MeTLStanza](u,p,r,h){
	private lazy val serializer = new MeTL2011XmlSerializer(configName)
	private lazy val config = ServerConfiguration.configForName(configName)

	override lazy val debug = true

	override def onMessageRecieved(room:String, messageType:String, message:MeTLStanza) = {
		println("recieved message: %s".format(message))
		config.getRoom(room) ! ServerToLocalMeTLStanza(message)
	}
	override def onUntypedMessageRecieved(room:String,message:String) = {
		println("recieved untyped message: %s".format(message))
		val parts = message.split(" ")
		config.getRoom(room) ! ServerToLocalMeTLStanza(MeTLCommand(config,"unknown",0L,parts.take(1)(0),parts.drop(1).toList))
	}
	override lazy val subscribedTypes = List("ink","textbox","image","dirtyInk","dirtyText","dirtyImage","submission","quiz","quizResponse","command").map(item => {
		val ser = (i:MeTLStanza) => {
			val xml = serializer.fromMeTLStanza(i) 
			println("xml: %s".format(xml))
			val messages = xml
			println("messages: %s".format(messages))
			val head = messages.headOption
			println("head: %s".format(head))
			head.map{
				case g:Group => g.nodes.headOption.getOrElse(NodeSeq.Empty)
				case e:Elem => e.child.headOption.getOrElse(NodeSeq.Empty)
			}.getOrElse(NodeSeq.Empty)
		}
		val deser = (s:NodeSeq) => serializer.toMeTLStanza(s)
		XmppDataType[MeTLStanza](item,ser,deser)
	})
}

class XmppMessageBus(jid:String,configName:String,hostname:String,username:String,password:String) extends MessageBus(jid,configName){
	lazy val xmpp = new MeTL2011XmppConn(username,password,"metlxConnector_%s_%s".format(username, new Date().getTime.toString),hostname,configName)
	override def sendStanzaToRoom(stanza:MeTLStanza) = stanza match {
		case i:MeTLInk => xmpp.sendMessage(jid,"ink",i)
		case t:MeTLText => xmpp.sendMessage(jid,"textbox",t)
		case i:MeTLImage => xmpp.sendMessage(jid,"image",i)
		case di:MeTLDirtyInk => xmpp.sendMessage(jid,"dirtyInk",di)
		case dt:MeTLDirtyText => xmpp.sendMessage(jid,"dirtyText",dt)
		case di:MeTLDirtyImage => xmpp.sendMessage(jid,"dirtyImage",di)
		case q:MeTLQuiz => xmpp.sendMessage(jid,"quiz",q)
		case qr:MeTLQuizResponse => xmpp.sendMessage(jid,"quizResponse",qr)
		case s:MeTLSubmission => xmpp.sendMessage(jid,"submission",s)
		case c:MeTLCommand => xmpp.sendMessage(jid,"command",c)
		case _ => {}
	}	
}
