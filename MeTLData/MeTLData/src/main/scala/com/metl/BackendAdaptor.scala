package com.metl.model

object ServerConfiguration{
	val empty = EmptyBackendAdaptor
	private var serverConfigs:List[ServerConfiguration] = List(EmptyBackendAdaptor)
	def setServerConfigurations(sc:List[ServerConfiguration]) = serverConfigs = sc
	def setDefaultServerConfiguration(f:() => ServerConfiguration) = defaultConfigFunc = f
	def configForName(name:String) = serverConfigs.find(c => c.name == name).getOrElse(default)
	def configForHost(host:String) = serverConfigs.find(c => c.host == host).getOrElse(default)	
	private var defaultConfigFunc = () => serverConfigs(0)
	def default = {
		defaultConfigFunc()
		//configForName("transientMadam")
//		val host = tryo(xml.XML.loadString(Http.getClient.getAsString("http://metl.adm.monash.edu/server.xml")).text).openOr("reifier.adm.monash.edu.au")
//		configForHost(host)
	}

}

abstract class ServerConfiguration(incomingName:String,incomingHost:String) {
	val name = incomingName
	val host = incomingHost
	protected val roomsProvider:RoomProvider
	def getRoomProvider:RoomProvider = roomsProvider
	def getMessageBus(id:String):MessageBus
	def getMessageBusForRoom(id:String,room:MeTLRoom):MessageBus
	def releaseMessageBus(id:String):Unit
	def releaseMessageBusForRoom(id:String,room:MeTLRoom):Unit
	def getRoom(jid:String):MeTLRoom = roomsProvider.get(jid)
	def getHistory(jid:String):History
	def sendStanzaToRoom(jid:String,stanza:MeTLStanza):Unit
	def searchForConversation(query:String):List[Conversation]
	def detailsOfConversation(jid:String):Conversation
	def createConversation(title:String):Conversation
	def updateConversation(jid:String,updatedConversation:Conversation):Conversation
	def getImage(jid:String,identity:String):MeTLImage
	def getResource(url:String):Array[Byte]
}

object EmptyBackendAdaptor extends ServerConfiguration("empty","empty"){
	val serializer = new PassthroughSerializer
	override val roomsProvider = EmptyRoomProvider
	override def getMessageBus(id:String) = EmptyMessageBus
	override def getMessageBusForRoom(id:String,room:MeTLRoom) = EmptyMessageBus
	override def releaseMessageBus(id:String) = {} 
	override def releaseMessageBusForRoom(id:String,room:MeTLRoom) = {}
	override def getHistory(jid:String) = History.empty
	override def sendStanzaToRoom(jid:String,stanza:MeTLStanza) = {}
	override def searchForConversation(query:String) = List.empty[Conversation]
	override def detailsOfConversation(jid:String) = Conversation.empty
	override def createConversation(title:String) = Conversation.empty
	override def updateConversation(jid:String,updatedConversation:Conversation) = Conversation.empty
	override def getImage(jid:String,identity:String) = MeTLImage.empty
	override def getResource(url:String) = Array.empty[Byte]
}

object FrontendSerializationAdaptor extends ServerConfiguration("frontend","frontend"){
	val serializer = new GenericXmlSerializer("frontend")
	override val roomsProvider = EmptyRoomProvider
	override def getMessageBus(id:String) = EmptyMessageBus
	override def getMessageBusForRoom(id:String,room:MeTLRoom) = EmptyMessageBus
	override def releaseMessageBus(id:String) = {} 
	override def releaseMessageBusForRoom(id:String,room:MeTLRoom) = {} 
	override def getHistory(jid:String) = History.empty
	override def sendStanzaToRoom(jid:String,stanza:MeTLStanza) = {}
	override def searchForConversation(query:String) = List.empty[Conversation]
	override def detailsOfConversation(jid:String) = Conversation.empty
	override def createConversation(title:String) = Conversation.empty
	override def updateConversation(jid:String,updatedConversation:Conversation) = Conversation.empty
	override def getImage(jid:String,identity:String) = MeTLImage.empty
	override def getResource(url:String) = Array.empty[Byte]
}
