package com.metl.embedded

import com.metl.data._
import com.metl.utils._

class EmbeddedPersistingMessageBusProvider(configName:String,dbInterface:LocalEmbeddedInterface) extends OneBusPerRoomMessageBusProvider {
	override def createNewMessageBus(d:MessageBusDefinition) = Stopwatch.time("EmbeddedPersistingMessageBusProvider.createNewMessageBus", () => {
		new EmbeddedPersistingLoopbackMessageBus(configName,d,dbInterface,this)
	})
}

class EmbeddedPersistingLoopbackMessageBus(configName:String,d:MessageBusDefinition,dbInterface:LocalEmbeddedInterface,provider:MessageBusProvider) extends MessageBus(d,provider){
	val jid = d.location
	override def sendStanzaToRoom(stanza:MeTLStanza) = Stopwatch.time("EmbeddedPersistingMessageBusProvider.sendStanzaToRoom", () => {
		storeStanzaInDB(stanza)
		recieveStanzaFromRoom(stanza)
		true
	})
	private def storeStanzaInDB(stanza:MeTLStanza):Unit = Stopwatch.time("EmbeddedPersistingMessageBusProvider.storeStanzaInMongo", () => {
		dbInterface.storeStanza(jid,stanza)
	})
}

