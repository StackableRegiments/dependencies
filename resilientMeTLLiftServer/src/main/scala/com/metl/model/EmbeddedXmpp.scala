package com.metl.model

import 
_root_.net.liftweb._
import util._
import Helpers._
import common._
import http._
import provider.servlet._

import org.apache.vysper.mina.TCPEndpoint
import org.apache.vysper.storage.StorageProviderRegistry
import org.apache.vysper.storage.inmemory.MemoryStorageProviderRegistry
import org.apache.vysper.xmpp.addressing.{Entity,EntityImpl}
import org.apache.vysper.xmpp.authorization.AccountManagement
import org.apache.vysper.xmpp.modules.extension.xep0054_vcardtemp.VcardTempModule
import org.apache.vysper.xmpp.modules.extension.xep0092_software_version.SoftwareVersionModule
import org.apache.vysper.xmpp.modules.extension.xep0119_xmppping.XmppPingModule
import org.apache.vysper.xmpp.modules.extension.xep0202_entity_time.EntityTimeModule
import org.apache.vysper.xmpp.server.XMPPServer


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

