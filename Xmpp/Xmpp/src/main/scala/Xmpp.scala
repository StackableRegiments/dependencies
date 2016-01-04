package com.metl.xmpp

import com.metl.utils._

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
import org.jivesoftware.smack.tcp._
import org.jivesoftware.smackx.iqregister._
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
		val xml = scala.xml.XML.loadString(input.toXML.toString)
		val result = deserialize(xml)
		result
	}
}
/*
class MeTLExtensionProvider extends PacketExtensionProvider {
	override def parseExtension(parser:XmlPullParser):PacketExtension = {
		val (elemName,xmlString) = parseTag(parser,"","")	
		new Payload(elemName,XmppUtils.ns,xmlString)
	}
	private def parseTag(parser:XmlPullParser,elementName:String,progress:String,depth:Int = 0):Tuple2[String,String] = {
		val (n,p,d) = parser.getEventType match {
			case XmlPullParser.END_DOCUMENT => {
				(elementName,progress,depth - 1)
			}
			case XmlPullParser.START_DOCUMENT => {
				(elementName,progress,depth + 1)
			}
			case XmlPullParser.START_TAG => {
				val name = parser.getName
				val newProgress = parser.getAttributeCount() match {
					case attCount:Int if (attCount > 0) => {
						val attributes = Range(0,attCount).foldLeft(List.empty[String])((acc,attIndex) => {
							var attributeString = ""
							parser.getAttributePrefix(attIndex) match {
								case attPref:String if (attPref.length > 0) => attributeString += "%s:".format(attPref)
								case _ => {}
							}
							parser.getAttributeName(attIndex) match {
								case attName:String if (attName.length > 0) => {
									parser.getAttributeValue(attIndex) match {
										case attValue:String if (attValue.length > 0) => {
											attributeString += "%s='%s'".format(attName,attValue)
										}
										case _ => {}
									}
								}
								case _ => {}
							}
							attributeString :: acc	
						})
						progress+"<"+name+" "+attributes.mkString(" ")+">"
					}
					case _ => progress+"<"+name+">"
				}
				val en = elementName match {
					case "" => name 
					case _ => elementName
				}
				(en,newProgress,depth + 1)
			}
			case XmlPullParser.END_TAG => {
				val newProgress = progress+"</"+parser.getName+">"
				(elementName,newProgress,depth - 1)
			}	
			case XmlPullParser.TEXT => {
				val newProgress = progress+parser.getText
				(elementName,newProgress,depth)
			}
			case _ => {
				(elementName,progress,depth)
			}
		}
		if (d < 1) {
			(n,p)
		} else {
			parser.next
			parseTag(parser,n,p,d)
		}
	}
}
*/

class MeTLExtensionProvider extends ExtensionElementProvider[ExtensionElement] with Logger{
  import org.jivesoftware.smackx.pubsub.SimplePayload
	protected def parseInternal(parser:XmlPullParser):ExtensionElement = {
		val (elemName,xmlString) = parseTag(parser,"","")	
		new SimplePayload(elemName,XmppUtils.ns,xmlString)
	}
	override def parse(parser:XmlPullParser,startingDepth:Int):ExtensionElement = {
    //not yet sure what this is meant to do, so just chaining it for the moment
    parseInternal(parser)
	}

	private def parseTag(parser:XmlPullParser,elementName:String,progress:String,depth:Int = 0):Tuple2[String,String] = {
		val (n,p,d) = parser.getEventType match {
			case XmlPullParser.END_DOCUMENT => {
				(elementName,progress,depth - 1)
			}
			case XmlPullParser.START_DOCUMENT => {
				(elementName,progress,depth + 1)
			}
			case XmlPullParser.START_TAG => {
				val name = parser.getName
				val newProgress = parser.getAttributeCount() match {
					case attCount:Int if (attCount > 0) => {
						val attributes = Range(0,attCount).foldLeft(List.empty[String])((acc,attIndex) => {
							var attributeString = ""
							parser.getAttributePrefix(attIndex) match {
								case attPref:String if (attPref.length > 0) => attributeString += "%s:".format(attPref)
								case _ => {}
							}
							parser.getAttributeName(attIndex) match {
								case attName:String if (attName.length > 0) => {
									parser.getAttributeValue(attIndex) match {
										case attValue:String if (attValue.length > 0) => {
											attributeString += "%s='%s'".format(attName,attValue)
										}
										case _ => {}
									}
								}
								case _ => {}
							}
							attributeString :: acc	
						})
						progress+"<"+name+" "+attributes.mkString(" ")+">"
					}
					case _ => progress+"<"+name+">"
				}
				val en = elementName match {
					case "" => name 
					case _ => elementName
				}
				(en,newProgress,depth + 1)
			}
			case XmlPullParser.END_TAG => {
				val newProgress = progress+"</"+parser.getName+">"
				(elementName,newProgress,depth - 1)
			}	
			case XmlPullParser.TEXT => {
				val newProgress = progress+parser.getText
				(elementName,newProgress,depth)
			}
			case _ => {
				(elementName,progress,depth)
			}
		}
		if (d < 1) {
			(n,p)
		} else {
			parser.next
			parseTag(parser,n,p,d)
		}
	}
}

object XmppUtils {
	//private val providerManager = org.jivesoftware.smack.provider.ProviderManager.getInstance
	import org.jivesoftware.smack.provider.ProviderManager
	private val packetExtensionProvider = new MeTLExtensionProvider
	val ns = "monash:metl"
	def possiblyAddExtensionProvider(elementName:String) = {
		if (ProviderManager.getExtensionProvider(elementName,ns) != packetExtensionProvider){
			ProviderManager.addExtensionProvider(elementName,ns,packetExtensionProvider)
		}
	}
}

class XmppConnectionManager(getConn:()=>Option[AbstractXMPPConnection],onConnectionLost:()=>Unit,onConnectionRegained:()=>Unit) extends ConnectionListener with Logger {
  override def authenticated(conn:XMPPConnection,resumed:Boolean):Unit = {
		debug("XMPPConnectionManager:connection authenticated (previous stream: %s)".format(resumed))
  }
  override def connected(conn:XMPPConnection):Unit = {
		debug("XMPPConnectionManager:connection connected")
  }
	override def connectionClosed():Unit = {
		debug("XMPPConnectionManager:connection closed")
		onConnectionLost()
	}
	override def connectionClosedOnError(e:Exception):Unit = {
		warn("XMPPConnectionManager:connection closed with error: "+e.getMessage)
		onConnectionLost()
	}
	override def reconnectingIn(seconds:Int):Unit = {
		trace("XMPPConnectionManager:reconnecting in... "+seconds)
	}
	override def reconnectionFailed(e:Exception):Unit = {
		warn("XMPPConnectionManager:reconnection failed: "+e.getMessage)
		onConnectionLost()
	}
	override def reconnectionSuccessful():Unit = {
		trace("XMPPConnectionManager:reconnection successful")
		onConnectionRegained()
	}
}

abstract class XmppConnection[T](credentialsFunc:() => Tuple2[String,String],incomingResource:String,incomingHost:String, incomingDomain:String, xmppConnection: Option[AbstractXMPPConnection],onConnectionLost:()=>Unit = () => {},onConnectionRegained:()=>Unit = () => {}) extends Logger {

  def this(credentailsFunc:() => Tuple2[String,String], incomingResource: String, incomingHost: String) {
        this(credentailsFunc, incomingResource, incomingHost, incomingHost, xmppConnection = None)
    }
  def this(credentialsFunc:() => Tuple2[String,String], incomingResource: String, incomingHost: String, incomingDomain:String) {
				this(credentialsFunc, incomingResource, incomingHost, incomingDomain, xmppConnection = None)
		}
	val host = incomingHost
	val domain = incomingDomain
  val credentials = credentialsFunc()
  val resource = incomingResource
  val username = credentials._1
  val password = credentials._2
//	Packet.setDefaultXmlns(XmppUtils.ns)

	protected def onConnLost:Unit = onConnectionLost()
	protected def onConnRegained:Unit = onConnectionRegained()

	def sendMessage(room:String,messageType:String,message:T):Unit = Stopwatch.time("XmppConnection.sendMessage",{			
		rooms.find(r => r._1 == room).map(r => {
			subscribedTypes.find(st => st.name == messageType).map(st => {
				val muc = r._2
				val roomMessage = muc.createMessage
				roomMessage.addExtension(st.generatePacketExtension(message))
				muc.sendMessage(roomMessage)
			})
		})
	})
	def sendSimpleMessage(room:String,message:String):Unit = Stopwatch.time("XmppConnection.sendSimpleMessage",{
		rooms.find(r => r._1 == room).map(r => {
			val muc = r._2
			val roomMessage = muc.createMessage
			roomMessage.setBody(message)
			muc.sendMessage(roomMessage)
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
	protected lazy val ignoredTypes:List[String] = List.empty[String]

	lazy val relevantElementNames = subscribedTypes.map(st => st.name).toList
	private var roomInterests:SynchronizedWriteMap[String,List[String]] = new SynchronizedWriteMap[String,List[String]]()
//	private var roomInterests:Map[String,List[String]] = Map.empty[String,List[String]]
	var rooms:Map[String,MultiUserChat] = Map.empty[String,MultiUserChat] 
	private var conn:Option[AbstractXMPPConnection] = None 
  private val additionalConnectionListener = new XmppConnectionManager(() => conn,onConnLost _, () => {
		conn.foreach(c => debug("room reconnected: %s -> (Connected:%s,Authenticated:%s)".format(resource,c.isConnected,c.isAuthenticated)))
		onConnRegained
		rooms.foreach(roomDefinition => {
			val roomJid = roomDefinition._1
			val room = roomDefinition._2
			debug("rejoining room %s on connection %s".format(roomJid,resource))
			room.leave
			room.join(resource)
		})
	})	

	private val config:XMPPTCPConnectionConfiguration = {
    import org.jivesoftware.smack.util.TLSUtils._
    val conf = XMPPTCPConnectionConfiguration.builder()
     .setServiceName(domain)
     .setHost(host)
     .setPort(port)
     .setResource(resource)
     .setUsernameAndPassword(username,password)
      .setDebuggerEnabled(debug)
      .setSendPresence(sendPresence)
     .setCompressionEnabled(allowCompression)
    disableHostnameVerificationForTlsCertificicates(acceptAllCertificates(conf)).build()
	}
	protected def initializeXmpp:Unit = Stopwatch.time("Xmpp.initializeXmpp", {
		relevantElementNames.foreach(ren => XmppUtils.possiblyAddExtensionProvider(ren))
		connectToXmpp
		val filter = new AndFilter( new PacketTypeFilter(classOf[Message]), new MessageTypeFilter(relevantElementNames))
		conn.map(c => c.addPacketListener(new RemoteSyncListener,filter))
	})
	initializeXmpp

	private def createXmppConnection: Option[AbstractXMPPConnection] = {
    xmppConnection match {
      case Some(xmpp) => Some(xmpp)
      case _ => tryo({
        val c = new XMPPTCPConnection(config)
        val rm = ReconnectionManager.getInstanceFor(c)
        rm.setReconnectionPolicy(ReconnectionManager.ReconnectionPolicy.FIXED_DELAY)
        rm.setFixedDelay(1) //this is measured in seconds
        rm.enableAutomaticReconnection
        c
      })
    }
	}

	def connectToXmpp:Unit = Stopwatch.time("Xmpp.connectToXmpp",{
		disconnectFromXmpp
		conn = createXmppConnection 
		conn.map(c => {
			c.connect
			c.addConnectionListener(additionalConnectionListener)
		})
		try {
			//conn.map(c => c.login(username,password,resource))
			conn.map(c => c.login())//username,password,resource))
		}
		catch {
			case e:XMPPException if (shouldAttemptRegistrationOnAuthFailed && e.getMessage.contains("not-authorized")) => {
				disconnectFromXmpp
				conn = createXmppConnection
				conn.map(c => {
					c.connect
					c.addConnectionListener(additionalConnectionListener)
					register
          c.login()
					//c.login(username,password,resource)
				})
			}
		}
	})

	def disconnectFromXmpp:Unit = Stopwatch.time("Xmpp.disconnectFromXmpp",{
		conn.map(c => {
			c.removeConnectionListener(additionalConnectionListener)
			c.disconnect(new Presence(Presence.Type.unavailable))
		})
	})

	protected def register:Unit = Stopwatch.time("Xmpp.register",{
		conn.map(c => {
			val accountManager = AccountManager.getInstance(c)
			accountManager.createAccount(username,password)
		})
	})

	protected def mucFor(room:String):Option[MultiUserChat] = Stopwatch.time("Xmpp.mucFor",{
		conn.map(c => {
			val roomJid = "%s@conference.%s".format(room,domain)
      val mucMan = MultiUserChatManager.getInstanceFor(c)
			mucMan.getMultiUserChat(roomJid)
		})
	})
	def joinRoom(room:String,interestId:String = ""):Option[MultiUserChat] = Stopwatch.time("Xmpp.joinRoom",{
		//debug("XMPP(%s):joinRoom(%s)".format(this.hashCode,room))
		val oldRoomInterests = roomInterests.getOrElseUpdate(room,List.empty[String])
		val creatingRoom = roomInterests(room) match {
			case l:List[String] if l.length > 0 => false
			case _ => true
		}
		roomInterests.update(room,interestId :: oldRoomInterests)
		if (creatingRoom){
			//debug("XMPP(%s):joinRoom.creatingRoom(%s)".format(this.hashCode,room))
			val roomJid = "%s@conference.%s".format(room,domain)
			conn.map(c => {
        val mucMan = MultiUserChatManager.getInstanceFor(c)
        val muc = mucMan.getMultiUserChat(roomJid)
				muc.join(resource)
				rooms = rooms.updated(room,muc)
				muc
			})
		}
		rooms.get(room)
	})
	def leaveRoom(roomName:String, interestId:String = ""):Unit = {
		roomInterests.getOrElseUpdate(roomName,List.empty[String]) match {
			case l:List[String] if l.length > 0 && l.contains(interestId) => {
				//debug("XMPP(%s):leaveRoom.removeInterestsFromExistingInterests".format(this))
				roomInterests.update(roomName,roomInterests(roomName).filterNot(_ == interestId))
			}
			case _ => {}
		}
		if (roomInterests(roomName).length == 0){
			//debug("XMPP(%s):leaveRoom".format(this))
			rooms.get(roomName).map(r => leaveRoom(r))
		}
	}
	def leaveRoom(room:MultiUserChat):Unit = {
		room.leave
		rooms = rooms.filterNot(r => r._2 == room)
	}
 	class MessageTypeFilter(predicates:List[String]) extends StanzaFilter with Logger{
    def accept(message:Stanza)= Stopwatch.time("Xmpp.MessageTypeFilter.accept",{
     	var smackMessage = message.asInstanceOf[Message]
			// added the getBody filter to ensure that messages that are commands (which is apparently necessary because they won't show up as extensions) are allowed through
      //List(smackMessage.getExtensions.toArray:_*).filter(ex => predicates.contains(ex.asInstanceOf[PacketExtension].getElementName)).length > 0
      List(smackMessage.getExtensions.toArray:_*).filter(ex => predicates.contains(ex.asInstanceOf[ExtensionElement].getElementName)).length > 0 || smackMessage.getBody().length > 0
    })
  }
  class RemoteSyncListener extends StanzaListener with Logger {
		def processPacket(packet:Stanza)= Stopwatch.time("Xmpp.RemoteSyncListener.processPacket",{
			val room = packet.getFrom.split("@").head
			if (List(packet.getExtensions.toArray:_*).map(e => {
				val ext = e.asInstanceOf[ExtensionElement]
				ext.getElementName match {
					case ignored:String if (ignoredTypes.exists(it => it.toLowerCase.trim == ignored.toLowerCase.trim)) => false
					case other:String if (relevantElementNames.contains(other)) => {
						subscribedTypes.find(st => st.name.toString.trim == other.toString.trim).map(st => {
							onMessageRecieved(room,other,st.comprehendResponse(packet))
							true
						}).getOrElse(false)
					}
					case other => {
						onUntypedMessageRecieved(room,ext.toXML.toString)	
						true
					}
				}
			}).filter(a => a == true).length == 0){
				val msg = packet.asInstanceOf[Message]
				onUntypedMessageRecieved(room,msg.getBody)
			}
		})
  }
}
