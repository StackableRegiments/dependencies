package com.metl.model

class MongoPersistingMessageBusProvider(configName:String,mongoHost:String,mongoPort:Int,mongoDb:String) extends MessageBusProvider(configName:String) {
	override def getMessageBus(jid:String) = {
		new MongoPersistingLoopbackMessageBus(jid,configName,mongoHost,mongoPort,mongoDb)
	}
}

class MongoPersistingLoopbackMessageBus(jid:String,configName:String,mongoHost:String,mongoPort:Int,mongoDb:String) extends MessageBus(jid,configName){
	private val mongo = new LocalMongoInterface(mongoHost,mongoPort,mongoDb)
	override def sendStanzaToRoom(stanza:MeTLStanza) = Stopwatch.time("MongoPersistingLoopbackMessageBus.sendStanzaToRoom", () => {
		storeStanzaInMongo(stanza)
		recieveStanzaFromRoom(stanza)
	})
	private def storeStanzaInMongo(stanza:MeTLStanza):Unit = Stopwatch.time("MongoPersistingLoopbackMessageBus.storeStanzaInMongo", () => {
//		serverConfig
//		mongo.
	})
}

