package com.metl.model

class MeTL2011BackendAdaptor(name:String,hostname:String,meggleUrl:String) extends ServerConfiguration(name,hostname){
	private val http = new SimpleAuthedHttpProvider("crying_horse","bacon_sandwich")
	private val conversations = new MeTL2011Conversations(name,meggleUrl,http,(c:Conversation) => {})
	private val history = new MeTL2011History(name,http)
	private val messageBusProvider = new XmppProvider(name,hostname,"metlXUsername","fred")
	val serializer = new MeTL2011XmlSerializer(name)
	override def getMessageBus(d:MessageBusDefinition) = messageBusProvider.getMessageBus(d)
	override def getHistory(jid:String) = history.getMeTLHistory(jid)
	override def searchForConversation(query:String) = conversations.search(query)
	override def detailsOfConversation(jid:String) = conversations.detailsOf(jid.toInt)
	override def createConversation(title:String) = conversations.createConversation(title)
	override def updateConversation(jid:String,updatedConversation:Conversation) = conversations.updateConversation(jid.toInt,updatedConversation)
	override def getImage(jid:String,identity:String) = history.getMeTLHistory(jid).getImageByIdentity(identity).getOrElse(MeTLImage.empty)
	override def getResource(url:String) = http.getClient.getAsBytes(url)
}

class TransientMeTL2011BackendAdaptor(name:String,hostname:String,meggleUrl:String) extends ServerConfiguration(name,hostname){
	private val http = new SimpleAuthedHttpProvider("crying_horse","bacon_sandwich")
	private val conversations = new MeTL2011Conversations(name,meggleUrl,http, (c:Conversation) => {})
	private val history = new MeTL2011History(name,http)
	private val messageBusProvider = new LoopbackMessageBusProvider
	val serializer = new MeTL2011XmlSerializer(name)
	override def getMessageBus(d:MessageBusDefinition) = messageBusProvider.getMessageBus(d)
	override def getHistory(jid:String) = history.getMeTLHistory(jid)
	override def searchForConversation(query:String) = conversations.search(query)
	override def detailsOfConversation(jid:String) = conversations.detailsOf(jid.toInt)
	override def createConversation(title:String) = Conversation.empty
	override def updateConversation(jid:String,updatedConversation:Conversation) = updatedConversation
	override def getImage(jid:String,identity:String) = history.getMeTLHistory(jid).getImageByIdentity(identity).getOrElse(MeTLImage.empty)
	override def getResource(url:String) = http.getClient.getAsBytes(url)
}
