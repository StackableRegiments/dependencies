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

import java.util.{ArrayList,List}
import org.apache.vysper.xmpp.addressing.{Entity,EntityFormatException,EntityImpl,EntityUtils}
import org.apache.vysper.xmpp.delivery.failure.{DeliveryException,IgnoreFailureStrategy}
import org.apache.vysper.xmpp.modules.DefaultDiscoAwareModule
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.MUCModule
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.handler.{MUCIqAdminHandler,MUCMessageHandler,MUCPresenceHandler}
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.model.{Conference,Occupant,Room}
import org.apache.vysper.xmpp.modules.extension.xep0045_muc.storage.{OccupantStorageProvider,RoomStorageProvider}
import org.apache.vysper.xmpp.modules.servicediscovery.management.{ComponentInfoRequestListener,InfoElement,InfoRequest,Item,ItemRequestListener,ServiceDiscoveryRequestException}
import org.apache.vysper.xmpp.protocol.{NamespaceURIs,StanzaProcessor}
import org.apache.vysper.xmpp.server.ServerRuntimeContext
import org.apache.vysper.xmpp.server.components.{Component,ComponentStanzaProcessor}
import org.apache.vysper.xmpp.stanza.{IQStanzaType,StanzaBuilder}
import org.slf4j.{Logger,LoggerFactory}

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
		processor.addHandler(new MUCMessageHandler(conference, fullDomain));
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

	override def addItemRequestListeners(itemRequestListeners:List[ItemRequestListener]):Unit = {
		itemRequestListeners.add(this)
	}
	def getComponentInfosFor(request:InfoRequest):List[InfoElement] = {
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
	override def addComponentInfoRequestListeners(componentInfoRequestListeners:List[ComponentInfoRequestListener]):Unit = {
		componentInfoRequestListeners.add(this)
	}

	def getItemsFor(request:InfoRequest):List[Item] = {
		//throws ServiceDiscoveryRequestException
		val to:Entity = request.getTo()
		if (to.getNode() == null) {
			if (fullDomain.equals(to)) {
				val conferenceItems:List[Item] = conference.getItemsFor(request)
				conferenceItems
			} else if (serverRuntimeContext.getServerEnitity().equals(to)) {
				val componentItem:List[Item] = new ArrayList[Item]()
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

