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
		val newMessage = stanza.adjustTimestamp(new java.util.Date().getTime)
		storeStanzaInDB(newMessage).map(m => {
			// this shares the message between all busses that reach this location, because in this case, they aren't connected to a higher level shared bus.
			provider.sendMessageToBus(b => b.location == d.location,m)
			recieveStanzaFromRoom(m)
			true
		}).getOrElse(false)
	})
	private def storeStanzaInDB(stanza:MeTLStanza):Option[MeTLStanza] = Stopwatch.time("EmbeddedPersistingMessageBusProvider.storeStanzaInDB", () => {
		dbInterface.storeStanza(jid,stanza)
	})
}

