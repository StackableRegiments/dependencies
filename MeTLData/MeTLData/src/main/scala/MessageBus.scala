package com.metl.data

import com.metl.utils._

// the feedback name should be bound to a particular onReceive function, so that we can use that feedbackName to match particular behaviours (given that the anonymous functions won't work that way for us)
class MessageBusDefinition(val location:String, val feedbackName:String, val onReceive:(MeTLStanza) => Unit = (s:MeTLStanza) => {}, val onConnectionLost:() => Unit = () => {}, val onConnectionReturned:() => Unit = () => {}){
	override def equals(other:Any):Boolean = {
		other match {	
			case omb:MessageBusDefinition => omb.location == location && omb.feedbackName == feedbackName
			case _ => false
		}
	}
	override def hashCode = (location+feedbackName).hashCode
}

abstract class MessageBusProvider {
	def getMessageBus(definition:MessageBusDefinition):MessageBus
	def releaseMessageBus(definition:MessageBusDefinition):Unit = {}
}
abstract class OneBusPerRoomMessageBusProvider extends MessageBusProvider {
	protected lazy val busses = new SynchronizedWriteMap[MessageBusDefinition,MessageBus](scala.collection.mutable.HashMap.empty[MessageBusDefinition,MessageBus],true,(k:MessageBusDefinition) => createNewMessageBus(k))
	protected def createNewMessageBus(definition:MessageBusDefinition):MessageBus
	override def getMessageBus(definition:MessageBusDefinition) = Stopwatch.time("OneBusPerRoomMessageBusProvider", () => busses.getOrElseUpdate(definition,createNewMessageBus(definition)))
	override def releaseMessageBus(definition:MessageBusDefinition) = Stopwatch.time("OneBusPerRoomMessageBusProvider", () => busses.remove(definition))
} 
class LoopbackMessageBusProvider extends OneBusPerRoomMessageBusProvider {
	override def createNewMessageBus(definition:MessageBusDefinition) = Stopwatch.time("LoopbackMessageBusProvider", () => new LoopbackBus(definition,this))
}
object EmptyMessageBusProvider extends MessageBusProvider{
	def getMessageBus(definition:MessageBusDefinition) = EmptyMessageBus
}

abstract class MessageBus(definition:MessageBusDefinition, creator:MessageBusProvider) {
	def sendStanzaToRoom(stanza:MeTLStanza):Unit
	def recieveStanzaFromRoom(stanza:MeTLStanza) = definition.onReceive(stanza)
	def notifyConnectionLost = definition.onConnectionLost()
	def notifyConnectionResumed = definition.onConnectionReturned()
	def release = creator.releaseMessageBus(definition)
}
class LoopbackBus(definition:MessageBusDefinition,creator:MessageBusProvider) extends MessageBus(definition,creator){
	override def sendStanzaToRoom(stanza:MeTLStanza) = recieveStanzaFromRoom(stanza)
}
object EmptyMessageBus extends MessageBus(new MessageBusDefinition("empty","throwaway"),EmptyMessageBusProvider){
	override def sendStanzaToRoom(stanza:MeTLStanza) = {}
	override def recieveStanzaFromRoom(stanza:MeTLStanza) = {}
}

