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

class XmppProvider(configName:String,hostname:String,username:String,password:String) extends OneBusPerRoomMessageBusProvider{
	override def createNewMessageBus(d:MessageBusDefinition) = Stopwatch.time("XmppProvider.createNewMessageBus", () => {
		new XmppMessageBus(configName,hostname,username,password,d,this)
	})
}

class MeTL2011XmppConn(u:String,p:String,r:String,h:String,configName:String,bus:MessageBus) extends XmppConnection[MeTLStanza](u,p,r,h){
	private lazy val serializer = new MeTL2011XmlSerializer(configName)
	private lazy val config = ServerConfiguration.configForName(configName)

	override lazy val debug = true

	override def onMessageRecieved(room:String, messageType:String, message:MeTLStanza) = {
		println("recieved for (%s) message: %s".format(room,message))
		bus.recieveStanzaFromRoom(message)
	}
	override def onUntypedMessageRecieved(room:String,message:String) = {
		println("recieved untyped message: %s".format(message))
		val parts = message.split(" ")
		bus.recieveStanzaFromRoom(MeTLCommand(config,"unknown",new java.util.Date().getTime,parts.head,parts.tail.toList))
	}
	override lazy val subscribedTypes = List("ink","textbox","image","dirtyInk","dirtyText","dirtyImage","submission","quiz","quizResponse","command","moveDelta").map(item => {
		val ser = (i:MeTLStanza) => {
			val xml = serializer.fromMeTLStanza(i) 
			val messages = xml
			val head = messages.headOption
			head.map{
				case g:Group => g.nodes.headOption.getOrElse(NodeSeq.Empty)
				case e:Elem => e.child.headOption.getOrElse(NodeSeq.Empty)
			}.getOrElse(NodeSeq.Empty)
		}
		val deser = (s:NodeSeq) => serializer.toMeTLStanza(s)
		XmppDataType[MeTLStanza](item,ser,deser)
	})
}

class XmppMessageBus(configName:String,hostname:String,username:String,password:String,d:MessageBusDefinition,creator:MessageBusProvider) extends MessageBus(d,creator){
	val jid = d.location
	lazy val xmpp = new MeTL2011XmppConn(username,password,"metlxConnector_%s_%s".format(username, new Date().getTime.toString),hostname,configName,this)
	xmpp.joinRoom(jid)
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
		case d:MeTLMoveDelta => xmpp.sendMessage(jid,"moveDelta",d)
		case _ => {}
	}	
	override def release = {
		xmpp.disconnectFromXmpp
		super.release
	}
}
