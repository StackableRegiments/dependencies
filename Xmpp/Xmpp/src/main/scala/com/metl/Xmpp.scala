package com.metl.model

import org.jivesoftware.smack._
import org.jivesoftware.smack.filter._
import org.jivesoftware.smack.packet._
import org.jivesoftware.smackx.muc._
import net.liftweb.actor._
import net.liftweb.common._
import java.util.Random
import net.liftweb.util.Helpers._
import org.xmlpull.v1.XmlPullParser
import org.jivesoftware.smack.provider._
import scala.xml._

class Payload(name:String,namespace:String,payload:String) extends PacketExtension {
	override def getNamespace = namespace
	override def getElementName = name
	override def toXML = payload
}

case class XmppDataType[T](elementName:String,serialize:(T) => NodeSeq,deserialize:(NodeSeq) => T){
	val name = elementName
	def generatePacketExtension(input:T):PacketExtension = {
		val payload = serialize(input).toString
		val parts = payload.split(">")
		val stitched = (parts.take(1)(0)+" xmlns='monash:metl' " :: parts.drop(1).toList).mkString(">")+">"
		val pay = new Payload(name,XmppUtils.ns,stitched)
		pay
	}
	def comprehendResponse(input:Packet):T = {
		val xml = scala.xml.XML.loadString(input.toXML)
		val result = deserialize(xml)
		result
	}
} 

class MeTLExtensionProvider extends PacketExtensionProvider {
	override def parseExtension(parser:XmlPullParser):PacketExtension = {
		val (elemName,xmlString) = parseTag(parser,"","")	
		println("parseExtension: (%s,%s)".format(elemName,xmlString))
		new Payload(elemName,XmppUtils.ns,xmlString)
	}
	private def parseTag(parser:XmlPullParser,elementName:String,progress:String,depth:Int = 0):Tuple2[String,String] = {
		var m = "xml d("+depth.toString+"): "
		val (n,p,d) = parser.getEventType match {
			case XmlPullParser.END_DOCUMENT => {
				m += "end_doc"
				(elementName,progress,depth - 1)
			}
			case XmlPullParser.START_DOCUMENT => {
				m += "start_doc"
				(elementName,progress,depth + 1)
			}
			case XmlPullParser.START_TAG => {
				m += "start_tag"
				val name = parser.getName
				val newProgress = progress+"<"+name+">"
				val en = elementName match {
					case "" => name 
					case _ => elementName
				}
				(en,newProgress,depth + 1)
			}
			case XmlPullParser.END_TAG => {
				m += "end_tag"
				val newProgress = progress+"</"+parser.getName+">"
				(elementName,newProgress,depth - 1)
			}	
			case XmlPullParser.TEXT => {
				m += "text"
				val newProgress = progress+parser.getText
				(elementName,newProgress,depth)
			}
			case _ => {
				m += "unknown"
				(elementName,progress,depth)
			}
		}
	//	println(m)
		if (d < 1) {
			(n,p)
		} else {
			parser.next
			parseTag(parser,n,p,d)
		}
	}
}

object XmppUtils {
	private val providerManager = org.jivesoftware.smack.provider.ProviderManager.getInstance
	private val packetExtensionProvider = new MeTLExtensionProvider
	val ns = "monash:metl"
	def possiblyAddExtensionProvider(elementName:String) = {
		if (providerManager.getExtensionProvider(elementName,ns) != packetExtensionProvider){
			providerManager.addExtensionProvider(elementName,ns,packetExtensionProvider)
		}
	}
}

abstract class XmppConnection[T](incomingUsername:String,password:String,incomingResource:String,incomingHost:String) {

	val host = incomingHost
	val username = incomingUsername
	val resource = incomingResource
	Packet.setDefaultXmlns(XmppUtils.ns)


	def sendMessage(room:String,messageType:String,message:T):Unit = Stopwatch.time("XmppConnection.sendMessage", () => {			
		rooms.find(r => r._1 == room).map(r => {
			subscribedTypes.find(st => st.name == messageType).map(st => {
				val muc = r._2
				val roomMessage = muc.createMessage
				roomMessage.addExtension(st.generatePacketExtension(message))
				println("XMPP-OUT: "+roomMessage.toXML)
				muc.sendMessage(roomMessage)
			})
		})
	})
	// override these to change settings on this connection
	protected	val port = 5222
	protected	val loadRosterAtLogin = false
	protected	val sendPresence = false
	protected	val acceptSelfSignedCerts = true
	protected	val allowReconnects = true 
	protected	val allowCompression = false
	protected	lazy val debug = false
	protected val shouldAttemptRegistrationOnAuthFailed = true
	// override this to do something with the messages you recieve
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
		relevantElementNames.foreach(ren => XmppUtils.possiblyAddExtensionProvider(ren))
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
     	var smackMessage = message.asInstanceOf[Message]
			// added the getBody filter to ensure that messages that are commands (which is apparently necessary because they won't show up as extensions) are allowed through
      //List(smackMessage.getExtensions.toArray:_*).filter(ex => predicates.contains(ex.asInstanceOf[PacketExtension].getElementName)).length > 0
      List(smackMessage.getExtensions.toArray:_*).filter(ex => predicates.contains(ex.asInstanceOf[PacketExtension].getElementName)).length > 0 || smackMessage.getBody().length > 0
    })
  }
  class RemoteSyncListener extends PacketListener{
		def processPacket(packet:Packet)= Stopwatch.time("Xmpp.RemoteSyncListener.processPacket", () => {
			println("XMPP-IN: "+packet.toXML)
			val room = packet.getFrom.split("@").head
			if (List(packet.getExtensions.toArray:_*).map(e => {
				val ext = e.asInstanceOf[PacketExtension]
				ext.getElementName match {
					case other:String if (relevantElementNames.contains(other)) => {
						println("XMPP-IN-EXT: "+ext.toXML)
						subscribedTypes.find(st => st.name.toString.trim == other.toString.trim).map(st => {
							onMessageRecieved(room,other,st.comprehendResponse(packet))
							true
						}).getOrElse(false)
					}
					case other => {
						println("XMPP-IN-?_EXT: "+ext.toXML)
						onUntypedMessageRecieved(room,ext.toXML)	
						true
					}
				}
			}).filter(a => a == true).length == 0){
				val msg = packet.asInstanceOf[Message]
				println("XMPP-IN-NO_EXT: "+msg.getBody)
				onUntypedMessageRecieved(room,msg.getBody)
			}
		})
  }
}

