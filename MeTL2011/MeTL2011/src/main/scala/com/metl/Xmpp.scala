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

class Payload(name:String,namespace:String,payload:String) extends PacketExtension {
	override def getNamespace = namespace
	override def getElementName = name
	override def toXML = payload
}

case class XmppDataType[T](elementName:String,serialize:(T) => NodeSeq,deserialize:(NodeSeq) => T){
	val name = elementName
	def generatePacketExtension(input:T):PacketExtension = {
		println("about to attempt to serializer: %s".format(input))
		val payload = serialize(input).toString
		val parts = payload.split(">")
		val stitched = (parts.take(1)(0)+" xmlns='monash:metl' " :: parts.drop(1).toList).mkString(">")+">"
		println("stitchedBackTogether: %s".format(stitched))
		println("gen'd %s from %s".format(payload,input))
		val pay = new Payload(name,"monash:metl",stitched)
		println("payload: %s".format(pay.getNamespace))
		pay
	}
	def comprehendResponse(input:Packet):T = {
		println("comprehendResponse[%s](%s)".format(this,input))
		val xml = scala.xml.XML.loadString(input.toXML)
		println("determined xml: %s".format(xml))
		val result = deserialize(xml)
		println("responseComprehended: %s".format(result))
		result
	}
} 

abstract class XmppConnection[T](incomingUsername:String,password:String,incomingResource:String,incomingHost:String) {

	val host = incomingHost
	val username = incomingUsername
	val resource = incomingResource

	Packet.setDefaultXmlns("monash:metl")

	def sendMessage(room:String,messageType:String,message:T):Unit = {			
		println("sending message: %s to %s".format(message,room))
		rooms.find(r => r._1 == room).map(r => {
			println("in room - sending message to room")
			subscribedTypes.find(st => st.name == messageType).map(st => {
				val muc = r._2
				val roomMessage = muc.createMessage
				roomMessage.addExtension(st.generatePacketExtension(message))
				println("constructed message: %s".format(roomMessage.getXmlns))
				muc.sendMessage(roomMessage)
			})
		})
	}
	// override these to change settings on this connection
	protected	val port = 5222
	protected	val loadRosterAtLogin = false
	protected	val sendPresence = false
	protected	val acceptSelfSignedCerts = true
	protected	val allowReconnects = true 
	protected	val allowCompression = false
	protected	lazy val debug = false
	protected val shouldAttemptRegistrationOnAuthFailed = true
	// override this to do something with the message you recieved
	protected def onMessageRecieved(room:String,messageType:String, message:T) 
	protected def onUntypedMessageRecieved(room:String,message:String)
	// override this to add messageTypes
	protected lazy val subscribedTypes:List[XmppDataType[T]] = List.empty[XmppDataType[T]]

	lazy val relevantElementNames = subscribedTypes.map(st => st.name).toList
	var rooms:Map[String,MultiUserChat] = Map.empty[String,MultiUserChat] 
	
	private var conn:Box[XMPPConnection] = Empty

	private val config:ConnectionConfiguration = {
		val c = new ConnectionConfiguration(host,port)
		c.setRosterLoadedAtLogin(loadRosterAtLogin)
		c.setSendPresence(sendPresence)
		c.setSelfSignedCertificateEnabled(acceptSelfSignedCerts)
		c.setReconnectionAllowed(allowReconnects)
		c.setCompressionEnabled(allowCompression)
		c.setDebuggerEnabled(debug)
		c
	}

	protected def initializeXmpp:Unit = Stopwatch.time("Xmpp.initializeXmpp", () => {
		connectToXmpp
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
			case e:XMPPException if (shouldAttemptRegistrationOnAuthFailed && e.getMessage.contains("not-authorized")) => {
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
			rooms = rooms.updated(room,muc)
			muc
		})
	})
	def leaveRoom(room:MultiUserChat):Unit = {
		room.leave
		rooms = rooms.filterNot(r => r._2 == room)
	}
 	class MessageTypeFilter(predicates:List[String]) extends PacketFilter{
    def accept(message:Packet)= Stopwatch.time("Xmpp.MessageTypeFilter.accept", () => {
			println("messages filtered on: "+predicates)
     	var smackMessage = message.asInstanceOf[Message]
      List(smackMessage.getExtensions.toArray:_*).filter(ex => predicates.contains(ex.asInstanceOf[PacketExtension].getElementName)).length > 0
    })
  }
  class RemoteSyncListener extends PacketListener{
		def processPacket(packet:Packet)= Stopwatch.time("Xmpp.RemoteSyncListener.processPacket", () => {
			List(packet.getExtensions.toArray:_*).map(e => {
				val ext = e.asInstanceOf[PacketExtension]
				println("packet recieved: "+packet)
				ext.getElementName match {
					case other:String if (relevantElementNames.contains(other)) => {
						println(other+": "+ext.toXML)
						println("looking for %s in subTypes %s".format(other,subscribedTypes.map(st => st.name)))
						subscribedTypes.find(st => st.name.toString.trim == other.toString.trim).map(st => {
							println("comprehending %s to %s as %s".format(packet,packet.getFrom,st))
							onMessageRecieved(packet.getFrom,other,st.comprehendResponse(packet))
						})
					}
					case other => {
						println(other+"(untyped): "+ext.toXML)
						onUntypedMessageRecieved(packet.getFrom,ext.toXML)	
					}
				}
			})
		})
  }
}

