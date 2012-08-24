package com.metl.model

abstract class MessageBusProvider(configName:String) {
	lazy val config = ServerConfiguration.configForName(configName)
	def getMessageBus(jid:String):MessageBus
	def getMessageBusForRoom(jid:String,room:MeTLRoom):MessageBus
	def releaseMessageBus(jid:String):Unit = {}
	def releaseMessageBusForRoom(jid:String,room:MeTLRoom):Unit = {}
}
abstract class OneBusPerRoomMessageBusProvider(configName:String) extends MessageBusProvider(configName:String) {
	protected lazy val busses = new SynchronizedWriteMap[String,MessageBus](scala.collection.mutable.HashMap.empty[String,MessageBus],true,(k:String) => createNewMessageBus(k,Some(config.getRoom(k))))
	protected def createNewMessageBus(jid:String,room:Option[MeTLRoom]):MessageBus 
	override def getMessageBus(jid:String) = Stopwatch.time("OneBusPerRoomMessageBusProvider", () => {
		busses.getOrElseUpdate(jid,createNewMessageBus(jid,Some(config.getRoom(jid))))
	})
	override def getMessageBusForRoom(jid:String,room:MeTLRoom) = Stopwatch.time("OneBusPerRoomMessageBusProvider", () => {
		busses.getOrElseUpdate(jid,createNewMessageBus(jid,Some(room)))
	})
	override def releaseMessageBus(jid:String) = Stopwatch.time("OneBusPerRoomMessageBusProvider", () => {
		busses.remove(jid)	
	})
	override def releaseMessageBusForRoom(jid:String,room:MeTLRoom) = Stopwatch.time("OneBusPerRoomMessageBusProvider", () => {
		busses.remove(jid)
	})
} 
class LoopbackMessageBusProvider(configName:String) extends OneBusPerRoomMessageBusProvider(configName:String) {
	override def createNewMessageBus(jid:String,room:Option[MeTLRoom]) = Stopwatch.time("LoopbackMessageBusProvider", () => {
		new LoopbackBus(jid,configName,room)
	})
}
class SingletonLoopbackMessageBusProvider(configName:String) extends MessageBusProvider(configName:String) {
	private val singleBus = new LoopbackBus("singleton",configName,None)
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
	def release = {
		serverConfig.releaseMessageBus(jid)
	}
}
class LoopbackBus(jid:String,configName:String,room:Option[MeTLRoom]) extends MessageBus(jid,configName,room){
	override def sendStanzaToRoom(stanza:MeTLStanza) = recieveStanzaFromRoom(stanza)
}

object EmptyMessageBus extends MessageBus("empty","empty"){
	override def sendStanzaToRoom(stanza:MeTLStanza) = {}
}

