package com.metl.embedded

import com.metl.utils._
import com.metl.data._

class LocalEmbeddedAdaptor(name:String) extends ServerConfiguration(name,"localhost"){
	protected val dbHost = "localhost"
	protected val dbPort = 27017
	protected val dbTable = "metlx"
	protected val dbInterface = new LocalEmbeddedInterface(dbHost,dbPort,dbTable)
	protected val messageBusProvider = new EmbeddedPersistingMessageBusProvider(name,dbInterface)
	protected val history = new EmbeddedHistory(name,dbInterface)
	protected val conversations = new EmbeddedConversations(name,dbInterface)
	protected val resourceProvider = new EmbeddedResourceProvider(name)
	val serializer = new EmbeddedSerializer(name)
	override def getMessageBus(d:MessageBusDefinition) = messageBusProvider.getMessageBus(d)
	override def getHistory(jid:String) = history.getMeTLHistory(jid)
	override def getConversationForSlide(slideJid:String) = conversations.conversationFor(slideJid.toInt).toString
	override def searchForConversation(query:String) = conversations.search(query)
	override def detailsOfConversation(jid:String) = conversations.detailsOf(jid.toInt)
	override def createConversation(title:String,author:String) = conversations.createConversation(title,author)
	override def deleteConversation(jid:String):Conversation = conversations.deleteConversation(jid)
	override def renameConversation(jid:String,newTitle:String):Conversation = conversations.renameConversation(jid,newTitle)
	override def changePermissions(jid:String,newPermissions:Permissions):Conversation = conversations.changePermissions(jid,newPermissions)
	override def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = conversations.updateSubjectOfConversation(jid,newSubject)
	override def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = conversations.addSlideAtIndexOfConversation(jid,index)
	override def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = conversations.reorderSlidesOfConversation(jid,newSlides)
	override def getImage(jid:String,identity:String) = history.getMeTLHistory(jid).getImageByIdentity(identity).getOrElse(MeTLImage.empty)
	override def getResource(url:String) = resourceProvider.getResource(url)
	override def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = resourceProvider.postResource(jid,userProposedId,data)
}
