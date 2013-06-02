package com.metl.model

import 
_root_.net.liftweb._
import util._
import Helpers._
import common._
import http._
import provider.servlet._

// for EmbeddedXmppServer
import org.apache.vysper.mina.TCPEndpoint
import org.apache.vysper.storage.StorageProviderRegistry
import org.apache.vysper.storage.inmemory.MemoryStorageProviderRegistry
import org.apache.vysper.xmpp.addressing.{Entity,EntityImpl}
import org.apache.vysper.xmpp.authorization.AccountManagement
import org.apache.vysper.xmpp.modules.extension.xep0054_vcardtemp.VcardTempModule
import org.apache.vysper.xmpp.modules.extension.xep0092_software_version.SoftwareVersionModule
import org.apache.vysper.xmpp.modules.extension.xep0119_xmppping.XmppPingModule
import org.apache.vysper.xmpp.modules.extension.xep0202_entity_time.EntityTimeModule
import org.apache.vysper.xmpp.modules.extension.xep0077_inbandreg.InBandRegistrationModule
import org.apache.vysper.xmpp.server.XMPPServer

// for MeTLMucModule

import java.util.{ArrayList => JavaArrayList,List => JavaList}
import org.apache.vysper.xmpp.addressing.{Entity,EntityFormatException,EntityImpl,EntityUtils}
//import org.apache.vysper.xmpp.delivery.failure.{DeliveryException,IgnoreFailureStrategy}
import org.apache.vysper.xmpp.modules.DefaultDiscoAwareModule
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.MUCModule
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.handler.{MUCIqAdminHandler,MUCMessageHandler,MUCPresenceHandler}
//import org.apache.vysper.xmpp.modules.extension.xep0045_muc.model.{Conference,Occupant,Room}
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.storage.{OccupantStorageProvider,RoomStorageProvider}
import org.apache.vysper.xmpp.modules.servicediscovery.management.{ComponentInfoRequestListener,InfoElement,InfoRequest,Item,ItemRequestListener,ServiceDiscoveryRequestException}
import org.apache.vysper.xmpp.protocol.{NamespaceURIs,StanzaProcessor}
import org.apache.vysper.xmpp.server.components.{Component,ComponentStanzaProcessor}
import org.apache.vysper.xmpp.stanza.{IQStanzaType,StanzaBuilder}
import org.slf4j.{Logger,LoggerFactory}

// for MeTLMUCMessageHandler

import org.apache.vysper.xml.fragment.{Attribute,XMLElement,XMLSemanticError}
//import org.apache.vysper.xmpp.addressing.{Entity,EntityFormatException,EntityImpl}}
import org.apache.vysper.xmpp.delivery.failure.{DeliveryException,IgnoreFailureStrategy}
import org.apache.vysper.xmpp.modules.core.base.handler.DefaultMessageHandler
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.MUCStanzaBuilder
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.dataforms.VoiceRequestForm
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.model.{Conference,Occupant,Role,Room,RoomType}
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.stanzas.{MucUserItem,X}
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.handler.MUCHandlerHelper
import org.apache.vysper.xmpp.server.{ServerRuntimeContext,SessionContext}
import org.apache.vysper.xmpp.stanza.{MessageStanza,MessageStanzaType,Stanza,StanzaBuilder,StanzaErrorCondition,StanzaErrorType}

class EmbeddedXmppServerRoomAdaptor(serverRuntimeContext:ServerRuntimeContext) {
	def relayMessageToMeTLRoom(location:String,message:AnyRef):Unit = {
	
	}
	def relayMessageToXmppMuc(location:String,message:AnyRef):Unit = {
	
	}
}

object EmbeddedXmppServer {
	protected var privateServer:Box[XMPPServer] = Empty
	def start = {
		println("embedded xmpp server start handler")
		val domain = "metl.adm.monash.edu.au"
		val providerRegistry = new MemoryStorageProviderRegistry()
		
		val accountManagement = providerRegistry.retrieve(classOf[AccountManagement]).asInstanceOf[AccountManagement]
		val user1 = EntityImpl.parse("dave@" + domain);
		if (!accountManagement.verifyAccountExists(user1)) 
			accountManagement.addUser(user1, "fred")

		privateServer = Full(new XMPPServer(domain))
		privateServer.map(p => {
			p.addEndpoint(new TCPEndpoint())
			p.setStorageProviderRegistry(providerRegistry)
			LiftRules.context match { 
     		case context: HTTPServletContext => {
					println("xmpp attaching cert")
					p.setTLSCertificateInfo(context.ctx.getResourceAsStream("WEB-INF/newSelfSingedCert.cer"),"fred")
				}
				case _ => throw new Exception("no certificate provided for the embedded xmpp server")
			}
			try {
				p.start()
				println("embedded xmpp server started")
				p.addModule(new SoftwareVersionModule())
				p.addModule(new EntityTimeModule())
				p.addModule(new VcardTempModule())
				p.addModule(new XmppPingModule())
				p.addModule(new InBandRegistrationModule())
				p.addModule(new MeTLMucModule())
				println("embedded xmpp default modules loaded")
			} catch {
				case e:Throwable => {
					throw e
				}
			}
		})
	}
	def sendMessageToRoom(message:AnyRef) = {}
}            

class MeTLMucModule(subdomain:String = "chat",conference:Conference = new Conference("Conference")) extends DefaultDiscoAwareModule with Component with ComponentInfoRequestListener with ItemRequestListener {
	protected var fullDomain:Entity = null
//	override final val logger:Logger = LoggerFactory.getLogger(classOf[MUCModule])
	protected var serverRuntimeContext:ServerRuntimeContext = null
	protected var stanzaProcessor:ComponentStanzaProcessor = null

	override def initialize(serverRuntimeContext:ServerRuntimeContext):Unit = {
		super.initialize(serverRuntimeContext)
		this.serverRuntimeContext = serverRuntimeContext
		fullDomain = EntityUtils.createComponentDomain(subdomain, serverRuntimeContext)
		val processor:ComponentStanzaProcessor = new ComponentStanzaProcessor(serverRuntimeContext)
		processor.addHandler(new MUCPresenceHandler(conference));
		processor.addHandler(new MeTLMUCMessageHandler(conference, fullDomain));
		processor.addHandler(new MUCIqAdminHandler(conference));
		stanzaProcessor = processor;
		val roomStorageProvider:RoomStorageProvider = serverRuntimeContext.getStorageProvider(classOf[RoomStorageProvider]).asInstanceOf[RoomStorageProvider]
		val occupantStorageProvider:OccupantStorageProvider = serverRuntimeContext.getStorageProvider(classOf[OccupantStorageProvider]).asInstanceOf[OccupantStorageProvider]
		if (roomStorageProvider == null) {
			//logger.warn("No room storage provider found, using the default (in memory)");
			println("No room storage provider found, using the default (in memory)");
		} else {
			conference.setRoomStorageProvider(roomStorageProvider);
		}
		if (occupantStorageProvider == null) {
			//logger.warn("No occupant storage provider found, using the default (in memory)");
			println("No occupant storage provider found, using the default (in memory)");
		} else {
			conference.setOccupantStorageProvider(occupantStorageProvider);
		}
		this.conference.initialize();
	}
	override def getName:String = "XEP-0045 Multi-user chat"
	override def getVersion:String = "1.24"

	override def addItemRequestListeners(itemRequestListeners:JavaList[ItemRequestListener]):Unit = {
		itemRequestListeners.add(this)
	}
	def getComponentInfosFor(request:InfoRequest):JavaList[InfoElement] = {
		//throws ServiceDiscoveryRequestException
		if (!fullDomain.getDomain().equals(request.getTo().getDomain())){
			null
		} else {
			if (request.getTo().getNode() == null) {
				conference.getServerInfosFor(request)
			} else {
				// might be an items request on a room
				val room:Room = conference.findRoom(request.getTo().getBareJID())
				if (room == null)
					null
				else {
					if (request.getTo().getResource() != null) {
						// request for an occupant
						val occupant:Occupant = room.findOccupantByNick(request.getTo().getResource())
						// request for occupant, relay
						if (occupant != null) {
							relayDiscoStanza(occupant.getJid(), request, NamespaceURIs.XEP0030_SERVICE_DISCOVERY_INFO);
						}
						null;
					} else {
						room.getInfosFor(request);
					}
				}
			}
		}
	}
	override def addComponentInfoRequestListeners(componentInfoRequestListeners:JavaList[ComponentInfoRequestListener]):Unit = {
		componentInfoRequestListeners.add(this)
	}

	def getItemsFor(request:InfoRequest):JavaList[Item] = {
		//throws ServiceDiscoveryRequestException
		val to:Entity = request.getTo()
		if (to.getNode() == null) {
			if (fullDomain.equals(to)) {
				val conferenceItems:JavaList[Item] = conference.getItemsFor(request)
				conferenceItems
			} else if (serverRuntimeContext.getServerEnitity().equals(to)) {
				val componentItem:JavaList[Item] = new JavaArrayList[Item]()
				componentItem.add(new Item(fullDomain))
				componentItem
			} else {
				null
			}
		} else if (fullDomain.getDomain().equals(to.getDomain())) {
			val room:Room = conference.findRoom(to.getBareJID())
			if (room != null) {
				if (to.getResource() != null) {
					val occupant:Occupant = room.findOccupantByNick(to.getResource())
					if (occupant != null) {
						relayDiscoStanza(occupant.getJid(), request, NamespaceURIs.XEP0030_SERVICE_DISCOVERY_ITEMS)
					}
					null
				} else {
					room.getItemsFor(request)
				}
			} else {
				null
			}
		} else {
			null
		}
	}
	protected def relayDiscoStanza(receiver:Entity, request:InfoRequest, ns:String):Unit = {
		val builder:StanzaBuilder = StanzaBuilder.createIQStanza(request.getFrom(), receiver, IQStanzaType.GET, request.getID())
		builder.startInnerElement("query", ns)
		if (request.getNode() != null) {
			builder.addAttribute("node", request.getNode())
		}
		try {
			serverRuntimeContext.getStanzaRelay().relay(receiver, builder.build(), new IgnoreFailureStrategy())
		} catch {
			case e:DeliveryException => {}
			case other => throw other
		}
	}
	def getSubdomain:String = subdomain
	def getStanzaProcessor:StanzaProcessor = stanzaProcessor
}

class MeTLMUCMessageHandler(conference:Conference,moduleDomain:Entity) extends DefaultMessageHandler {

	//final Logger logger = LoggerFactory.getLogger(MUCMessageHandler.class);

	override protected def verifyNamespace(stanza:Stanza):Boolean = true
	private def createMessageErrorStanza(from:Entity,to:Entity,id:String, typeName:StanzaErrorType, errorCondition:StanzaErrorCondition, stanza:Stanza):Stanza = {
		MUCHandlerHelper.createErrorStanza("message", NamespaceURIs.JABBER_CLIENT, from, to, id, typeName.value(), errorCondition.value(), stanza.getInnerElements())
	}
	override protected def executeMessageLogic(stanza:MessageStanza, serverRuntimeContext:ServerRuntimeContext, sessionContext:SessionContext) = {
		println("Received message for MUC")
		val from:Entity = stanza.getFrom()
		val roomWithNickJid = stanza.getTo()
		val roomJid = roomWithNickJid.getBareJID()
		val typeName:MessageStanzaType = stanza.getMessageType()

		if (typeName != null && typeName == MessageStanzaType.GROUPCHAT) {
			// groupchat, message to a room
			// must not have a nick
			if (roomWithNickJid.getResource() != null) {
			 createMessageErrorStanza(roomJid, from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.BAD_REQUEST, stanza)
			} else {
				println("Received groupchat message to %s".format(roomJid))
				val room:Room = conference.findRoom(roomJid)
				if (room != null) {
					val sendingOccupant:Occupant = room.findOccupantByJID(from)
					// sender must be participant in room
					if (sendingOccupant != null) {
						val roomAndSendingNick:Entity = new EntityImpl(room.getJID(), sendingOccupant.getNick())
						if (sendingOccupant.hasVoice()) {
							// relay message to all occupants in room
							if (stanza.getSubjects() != null && !stanza.getSubjects().isEmpty()) {
								try {
								// subject message
									if (!room.isRoomType(RoomType.OpenSubject) && !sendingOccupant.isModerator()) {
										// room only allows moderators to change the subject, and sender is not a moderator
										createMessageErrorStanza(room.getJID(), from, stanza.getID(), StanzaErrorType.AUTH, StanzaErrorCondition.FORBIDDEN, stanza)
									} else {
										null
									}
								} catch {
									case e:XMLSemanticError => {
								// not a subject message, ignore exception
										null
									}
								}
							} else {
								println("Relaying message to all room occupants")
								for (occupant:Occupant <- room.getOccupants().toArray().toList.asInstanceOf[List[Occupant]]) {
									println("Relaying message to %s".format(occupant))
									val replaceAttributes:JavaList[Attribute] = new JavaArrayList[Attribute]()
									replaceAttributes.add(new Attribute("from", roomAndSendingNick.getFullQualifiedName()))
									replaceAttributes.add(new Attribute("to",occupant.getJid().getFullQualifiedName()))
									relayStanza(occupant.getJid(), StanzaBuilder.createClone(stanza, true, replaceAttributes).build(), serverRuntimeContext)
								}
								room.getHistory().append(stanza, sendingOccupant)
								null
							}
						} else {
							createMessageErrorStanza(room.getJID(), from, stanza.getID(), StanzaErrorType.MODIFY,StanzaErrorCondition.FORBIDDEN, stanza)
						}
					} else {
						createMessageErrorStanza(room.getJID(), from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.NOT_ACCEPTABLE, stanza)
					}
				} else {
					createMessageErrorStanza(moduleDomain, from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.ITEM_NOT_FOUND, stanza)
				}
			}
		} else if (typeName == null || typeName == MessageStanzaType.CHAT || typeName == MessageStanzaType.NORMAL) {
			//private message
			println("Received direct message to %s".format(roomWithNickJid))
			val room:Room = conference.findRoom(roomJid)
			if (room != null) {
				val sendingOccupant:Occupant = room.findOccupantByJID(from)
				// sender must be participant in room
				if (roomWithNickJid.equals(roomJid)) {
					// check x element
					if (stanza.getVerifier().onlySubelementEquals("x", NamespaceURIs.JABBER_X_DATA)) {
						// void requests
						println("Received voice request for room %s".format(roomJid))
						handleVoiceRequest(from, sendingOccupant, room, stanza, serverRuntimeContext)
						null
					} else if (stanza.getVerifier().onlySubelementEquals("x", NamespaceURIs.XEP0045_MUC_USER)){
						//invites/declines
						handleInvites(stanza, from, sendingOccupant, room, serverRuntimeContext)
					} else {
						//do something here
						null
					}
				} else if (roomWithNickJid.isResourceSet()){
					if (sendingOccupant != null){
						// got resource, private message for occupant
						val receivingOccupant:Occupant = room.findOccupantByNick(roomWithNickJid.getResource())
						// must be sent to an existing occupant in the room
						if (receivingOccupant != null) {
							val roomAndSendingNick:Entity = new EntityImpl(room.getJID(), sendingOccupant.getNick())
							println("Relaying message to %s".format(receivingOccupant))
							val replaceAttributes:JavaList[Attribute] = new JavaArrayList[Attribute]()
							replaceAttributes.add(new Attribute("from", roomAndSendingNick.getFullQualifiedName()))
							replaceAttributes.add(new Attribute("to", receivingOccupant.getJid().getFullQualifiedName()))
							relayStanza(receivingOccupant.getJid(), StanzaBuilder.createClone(stanza, true, replaceAttributes).build(), serverRuntimeContext)
							null
						} else {
							// TODO correct error?
							createMessageErrorStanza(moduleDomain, from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.ITEM_NOT_FOUND, stanza)
						}
					} else {
						// user must be occupant to send direct message
						createMessageErrorStanza(room.getJID(), from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.NOT_ACCEPTABLE, stanza)
					}
				} else {
					createMessageErrorStanza(moduleDomain, from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.ITEM_NOT_FOUND, stanza)
				}
			} else {
				createMessageErrorStanza(moduleDomain, from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.ITEM_NOT_FOUND, stanza)
			}
		} else {
			null
		}
	}
	protected def handleInvites(stanza:MessageStanza, from:Entity, sendingOccupant:Occupant, room:Room, serverRuntimeContext:ServerRuntimeContext):Stanza = {
		val x:X = X.fromStanza(stanza)
		if (x != null && x.getInvite() != null) {
			if (sendingOccupant != null) {
				// invite, forward modified invite
				try {
					val invite:Stanza = MUCHandlerHelper.createInviteMessageStanza(stanza, room.getPassword());
					relayStanza(invite.getTo(), invite, serverRuntimeContext);
					null
				} catch {
					case e:EntityFormatException => {
						// invalid format of invite element
						createMessageErrorStanza(room.getJID(), from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.JID_MALFORMED, stanza)
					}
					case other => throw other
				}
			} else {
				// user must be occupant to send invite
				createMessageErrorStanza(room.getJID(), from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.NOT_ACCEPTABLE, stanza)
			}
		} else if (x != null && x.getDecline() != null) {
			// invite, forward modified decline
			try {
				val decline:Stanza = MUCHandlerHelper.createDeclineMessageStanza(stanza);
				relayStanza(decline.getTo(), decline, serverRuntimeContext);
				null
			} catch {
				case e:EntityFormatException => {
					// invalid format of invite element
					createMessageErrorStanza(room.getJID(), from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.JID_MALFORMED, stanza)
				}
				case other => throw other
			}
		} else {
			createMessageErrorStanza(room.getJID(), from, stanza.getID(), StanzaErrorType.MODIFY, StanzaErrorCondition.UNEXPECTED_REQUEST, stanza)
		}
	}
	protected def handleVoiceRequest(from:Entity, sendingOccupant:Occupant, room:Room, stanza:Stanza, serverRuntimeContext:ServerRuntimeContext):Unit = {
		val dataXs:JavaList[XMLElement] = stanza.getInnerElementsNamed("x", NamespaceURIs.JABBER_X_DATA)
		val dataX:XMLElement = dataXs.get(0)
		//check if "request_allow" is set
		val fields:JavaList[XMLElement] = dataX.getInnerElementsNamed("field", NamespaceURIs.JABBER_X_DATA)
		val requestAllow:String = getFieldValue(fields, "muc#request_allow")
		if ("true".equals(requestAllow)) {
			//submitted voice grant, only allowed by moderators
			if (sendingOccupant.isModerator()) {
				val requestNick:String = getFieldValue(fields, "muc#roomnick")
				val requestor:Occupant = room.findOccupantByNick(requestNick)
				requestor.setRole(Role.Participant)
				//nofity remaining users that user got role updated
				val presenceItem:MucUserItem = new MucUserItem(requestor.getAffiliation(), requestor.getRole())
				for (occupant:Occupant <- room.getOccupants().toArray().toList.asInstanceOf[List[Occupant]]) {
					val presenceToRemaining:Stanza = MUCStanzaBuilder.createPresenceStanza(requestor.getJidInRoom(), occupant.getJid(), null, NamespaceURIs.XEP0045_MUC_USER, presenceItem)
					relayStanza(occupant.getJid(), presenceToRemaining, serverRuntimeContext)
				}
			}
		} else if (requestAllow == null) {
			// no request allow, treat as voice request
			val requestForm:VoiceRequestForm = new VoiceRequestForm(from, sendingOccupant.getNick())
			for (moderator:Occupant <- room.getModerators().toArray().toList.asInstanceOf[List[Occupant]]) {
				val request:Stanza = StanzaBuilder.createMessageStanza(room.getJID(), moderator.getJid(), null, null).addPreparedElement(requestForm.createFormXML()).build()
				relayStanza(moderator.getJid(), request, serverRuntimeContext)
			}
		}
	}
	protected def getFieldValue(fields:JavaList[XMLElement], varName:String):String = {
		var found:Boolean = false
		var output:String = null
		for (field:XMLElement <- fields.toArray().toList.asInstanceOf[List[XMLElement]] if !found){
			if (varName.equals(field.getAttributeValue("var"))){
				try {
					output = field.getSingleInnerElementsNamed("value", NamespaceURIs.JABBER_X_DATA).getInnerText().getText()
					found = true
				} catch {
					case e:XMLSemanticError => {
						null
					}
					case other => throw other
				}
			}
		} 
		output
	}    
	protected def relayStanza(receiver:Entity, stanza:Stanza, serverRuntimeContext:ServerRuntimeContext):Unit = {
		try {
			serverRuntimeContext.getStanzaRelay().relay(receiver, stanza, new IgnoreFailureStrategy())
		} catch {
			case e:DeliveryException => {
				println("presence relaying failed %s".format(e))
			}
			case other => throw other
		}
	}
}

