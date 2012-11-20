package com.metl.persisted

import com.metl.data._
import com.metl.utils._

class PersistingMessageBusProvider(configName:String,dbInterface:PersistenceInterface) extends OneBusPerRoomMessageBusProvider {
	override def createNewMessageBus(d:MessageBusDefinition) = Stopwatch.time("EmbeddedPersistingMessageBusProvider.createNewMessageBus", () => {
		new PersistingLoopbackMessageBus(configName,d,dbInterface,this)
	})
}

class PersistingLoopbackMessageBus(configName:String,d:MessageBusDefinition,dbInterface:PersistenceInterface,provider:MessageBusProvider) extends MessageBus(d,provider){
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

