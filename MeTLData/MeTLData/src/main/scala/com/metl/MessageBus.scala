package com.metl.model

abstract class MessageBusProvider(configName:String) {
	lazy val config = ServerConfiguration.configForName(configName)
	def getMessageBus(jid:String):MessageBus
	def getMessageBusForRoom(jid:String,room:MeTLRoom):MessageBus
}
class LoopbackMessageBusProvider(configName:String) extends MessageBusProvider(configName:String) {
	private val singleBus = new LoopbackBus("singleton",configName)
	override def getMessageBus(jid:String) = singleBus
	override def getMessageBusForRoom(jid:String,room:MeTLRoom) = singleBus
}
abstract class MessageBus(jid:String, configName:String, room:Option[MeTLRoom] = None) {
	lazy val serverConfig = ServerConfiguration.configForName(configName)
	def sendStanzaToRoom(stanza:MeTLStanza):Unit
	def recieveStanzaFromRoom(stanza:MeTLStanza):Unit = {
		room.getOrElse(stanza match {
			case m:MeTLCanvasContent => serverConfig.getRoom(m.slide)
			case _ => EmptyRoom
		}) ! ServerToLocalMeTLStanza(stanza)
	}
}
class LoopbackBus(jid:String,configName:String) extends MessageBus(jid,configName){
	override def sendStanzaToRoom(stanza:MeTLStanza) = recieveStanzaFromRoom(stanza)
}

object EmptyMessageBus extends MessageBus("empty","empty"){
	override def sendStanzaToRoom(stanza:MeTLStanza) = {}
}

