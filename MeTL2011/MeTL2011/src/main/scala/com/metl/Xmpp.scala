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

class FakeXmpp extends Xmpp("no_username","no_password","localFakeXmpp","no_room") {
	override def connectToXmpp:Unit = {}
	override def disconnectFromXmpp:Unit = {}
	override val resource:String = "no_resource"

	override def notifyRoomOfMeTLStanza(s:MeTLStanza):Unit = Stopwatch.time("FakeXmpp.notifyRoomOfMeTLStanza", () => {
		updateLocalMeTLStanza(new XMPPMeTLStanzaAnnouncer(s))
	})
	override def initializeXmpp:Unit = {}
	override def register:Unit = {}
	override def mucFor(roomName:String) = Empty
	override def joinRoom(roomName:String):Box[MultiUserChat] = Empty
	override def leaveRoom(room:MultiUserChat):Unit = {}
}

class Xmpp(username:String,password:String,incomingHost:String, incomingRoomName:String, serverConfig:ServerConfiguration = ServerConfiguration.default) {
	val host = incomingHost
	val roomName = incomingRoomName
	var room:Box[MultiUserChat] = Empty 
	protected val resource:String = "metlxConnector_%s_%s".format(roomName,new java.util.Date().getTime.toString)
	private var conn:Box[XMPPConnection] = Empty
	private val config:ConnectionConfiguration ={
		val port = 5222
		val loadRosterAtLogin = false
		val sendPresence = false
		val acceptSelfSignedCerts = true
		val allowReconnects = true 
		val allowCompression = false
		val debug = false

		val c = new ConnectionConfiguration(host,port)
		c.setRosterLoadedAtLogin(loadRosterAtLogin)
		c.setSendPresence(sendPresence)
		c.setSelfSignedCertificateEnabled(acceptSelfSignedCerts)
		c.setReconnectionAllowed(allowReconnects)
		c.setCompressionEnabled(allowCompression)
		c.setDebuggerEnabled(debug)
		c
	}
	val relevantElementNames = List("metlDataAnnouncer","ink","dirtyInk","image","dirtyImage","genericStanza","genericCanvasContent","submission","quiz","quizResponse","command")
	protected def initializeXmpp:Unit = Stopwatch.time("Xmpp.initializeXmpp", () => {
		connectToXmpp
		room = joinRoom(roomName)
		val filter = new AndFilter( new PacketTypeFilter(classOf[Message]), new MessageTypeFilter(relevantElementNames))
		conn.map(c => c.addPacketListener(new RemoteSyncListener,filter))
	})
	initializeXmpp

	def connectToXmpp:Unit = Stopwatch.time("Xmpp.connectToXmpp", () => {
		disconnectFromXmpp
		conn = tryo(new XMPPConnection(config))
		conn.map(c => c.connect)
		try {
			conn.map(c => c.login(username,password,resource))
		}
		catch {
			case e:XMPPException if (e.getMessage.contains("not-authorized")) => {
				disconnectFromXmpp
				conn = tryo(new XMPPConnection(config))
				conn.map(c => {
					c.connect
					register
					c.login(username,password,resource)
				})
			}
		}
	})

	def disconnectFromXmpp:Unit = Stopwatch.time("Xmpp.disconnectFromXmpp", () => {
		conn.map(c => c.disconnect(new Presence(Presence.Type.unavailable)))
	})

	protected def register:Unit = Stopwatch.time("Xmpp.register", () => {
		conn.map(c => {
			val accountManager = c.getAccountManager
			accountManager.createAccount(username,password)
		})
	})

	protected def mucFor(room:String):Box[MultiUserChat] = Stopwatch.time("Xmpp.mucFor", () => {
		conn.map(c => {
			val roomJid = "%s@conference.%s".format(room,host)
			new MultiUserChat(c,roomJid)
		})
	})
	def joinRoom(room:String):Box[MultiUserChat] = Stopwatch.time("Xmpp.joinRoom", () => {
		val roomJid = "%s@conference.%s".format(room,host)
		conn.map(c => {
			val muc = new MultiUserChat(c,roomJid)
			muc.join(resource)
			muc
		})
	})
	def notifyRoomOfMeTLStanza(s:MeTLStanza):Unit = Stopwatch.time("Xmpp.notifyRoomOfMeTLStanza", () => {
		room.map( r => {
			val message = r.createMessage
			val msa = new XMPPMeTLStanzaAnnouncer(s)
			message.addExtension(msa)
			r.sendMessage(message)
		})
	})
	def leaveRoom(room:MultiUserChat):Unit = {
		room.leave
	}
 	class MessageTypeFilter(predicates:List[String]) extends PacketFilter{
    def accept(message:Packet)= Stopwatch.time("Xmpp.MessageTypeFilter.accept", () => {
     	var smackMessage = message.asInstanceOf[Message]
      List(smackMessage.getExtensions.toArray:_*).filter(ex => predicates.contains(ex.asInstanceOf[PacketExtension].getElementName)).length > 0
    })
  }

  class RemoteSyncListener extends PacketListener{
		def processPacket(packet:Packet)= Stopwatch.time("Xmpp.RemoteSyncListener.processPacket", () => {
			List(packet.getExtensions.toArray:_*).map(e => {
				val ext = e.asInstanceOf[PacketExtension]
				ext.getElementName match {
					case other:String if (relevantElementNames.contains(other)) => {
						XMPPMeTLStanzaAnnouncer.fromXMLString(serverConfig,ext.toXML).map(updateLocalMeTLStanza _)
					}
					case other => {
						println("Xmpp:QuestionAnnouncerListener:processPacket:getExtensions returned unknown extension: %s".format(other.toString))
					}
				}
			})
		})
  }
	def updateLocalMeTLStanza(msa:XMPPMeTLStanzaAnnouncer) = Stopwatch.time("Xmpp.updateLocalMeTLStanza", () => {
		msa.stanza match {
			case m:MeTLCanvasContent => serverConfig.getMessageBus(m.slide).recieveStanzaFromRoom(m)
		}
	})
}


object XMPPMeTLStanzaAnnouncer {
	private val serializer = new GenericXmlSerializer("xmpp")
	def fromXMLString(server:ServerConfiguration,xString:String):Box[XMPPMeTLStanzaAnnouncer] = Stopwatch.time("XmppMeTLStanzaAnnouncer", () => tryo({
		new XMPPMeTLStanzaAnnouncer(serializer.toMeTLStanza(scala.xml.XML.loadString(xString)))
	}))
	def fromStanza(s:MeTLStanza):Box[XMPPMeTLStanzaAnnouncer] = Stopwatch.time("XmppMeTLStanzaAnnouncer.fromStanza", () => tryo({
		new XMPPMeTLStanzaAnnouncer(s)
	}))
}

class XMPPMeTLStanzaAnnouncer(incomingStanza:MeTLStanza) extends PacketExtension{
	val stanza = incomingStanza
	override val getNamespace = "monash:metl"
	override val getElementName = stanza match {
		case i:MeTLInk => "ink"
		case t:MeTLText => "text"
		case i:MeTLImage => "image"
		case di:MeTLDirtyInk => "dirtyInk"
		case dt:MeTLDirtyText => "dirtyText"
		case di:MeTLDirtyImage => "dirtyImage"
		case c:MeTLCommand => "command"
		case q:MeTLQuiz => "quiz"
		case qr:MeTLQuizResponse => "quizResponse"
		case s:MeTLSubmission => "submission"
		case _ => "metl"
	}
	override val toXML:String = Stopwatch.time("XmppMeTLStanzaAnnouncer.toXml", () => (XMPPMeTLStanzaAnnouncer.serializer.fromMeTLStanza(stanza) \\ "message").headOption.map(m => m.asInstanceOf[Elem].child.toString).getOrElse(""))
}

case class InitializeXMPP(server:String,username:String,password:String,room:String)
case object InitializeFakeXMPP

case class MeTLStanzaSyncRequest(m:MeTLStanza)

class XMPPSyncActor extends LiftActor {
	private var xmpp:Box[Xmpp] = Empty
	override def messageHandler = {
		case MeTLStanzaSyncRequest(stanza) => Stopwatch.time("XmppSyncActor.MeTLStanzaSyncRequest", () => xmpp.map(x => x.notifyRoomOfMeTLStanza(stanza)))
		case InitializeXMPP(server,username,password,room) => Stopwatch.time("XmppSyncActor.InitializeXmpp", () => {
      xmpp match {
				case Full(x) => x.disconnectFromXmpp
				case _ => {}
			}
      xmpp = Full(new Xmpp(username,password,server,room))
      xmpp.map(x => println("starting XMPPActor for %s@%s".format(x.roomName,x.host)))
    })
		case InitializeFakeXMPP => Stopwatch.time("XmppSyncActor.InitializeFakeXmpp", () => {
			xmpp match {
				case Full(x) => x.disconnectFromXmpp
				case _ => {}
			}
			xmpp = Full(new FakeXmpp)
		})
		case other => println("XMPPSyncActor (%s) received unknown message: %s".format(xmpp.map(x => x.roomName).openOr("unknown room"),other))
	}
}
