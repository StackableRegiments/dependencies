package com.metl.model

class LocalMongoAdaptor(name:String) extends ServerConfiguration(name,"localhost"){
	private val mongoHost = "localhost"
	private val mongoPort = 27017
	private val mongoDB = "metlx"
	private val messageBusProvider = new MongoPersistingMessageBusProvider(name,mongoHost,mongoPort,mongoDB)
	private val history = new MongoHistory(name,mongoHost,mongoPort,mongoDB)
	private val conversations = new MongoConversations(name,mongoHost,mongoPort,mongoDB)
	val serializer = new MongoSerializer(name)
	override val roomsProvider = new HistoryCachingRoomProvider(name)
	override def getMessageBus(id:String) = messageBusProvider.getMessageBus(id)
	override def getHistory(jid:String) = history.getMeTLHistory(jid)
	override def sendStanzaToRoom(jid:String,stanza:MeTLStanza) = getMessageBus(jid).sendStanzaToRoom(stanza)
	override def searchForConversation(query:String) = conversations.search(query)
	override def detailsOfConversation(jid:String) = conversations.detailsOf(jid.toInt)
	override def createConversation(title:String) = conversations.createConversation(title) 
	override def updateConversation(jid:String,updatedConversation:Conversation) = conversations.updateConversation(jid.toInt,updatedConversation)
	override def getImage(jid:String,identity:String) = getRoom(jid).getHistory.getImageByIdentity(identity).getOrElse(MeTLImage.empty)
	override def getResource(url:String) = Array.empty[Byte]
}
