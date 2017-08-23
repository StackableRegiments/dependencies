package com.metl.persisted

import com.metl.data._

abstract class PersistedAdaptor(name:String,host:String) extends ServerConfiguration(name,host){
  protected val dbInterface:PersistenceInterface
  protected lazy val history = new PersistedHistory(this,dbInterface)
  protected lazy val conversations = new PersistedConversations(this,dbInterface)
  protected lazy val resourceProvider = new PersistedResourceProvider(this,dbInterface)
  override def shutdown = {
    dbInterface.shutdown
    super.shutdown
  }
  override def isReady = {
    dbInterface.isReady
    super.isReady
  }
  override def getHistory(jid:String) = history.getMeTLHistory(jid)
  override def getConversationForSlide(slideJid:String) = conversations.conversationFor(slideJid.toInt).toString
  override def searchForConversation(query:String) = conversations.search(query)
  override def detailsOfConversation(jid:String) = conversations.detailsOf(jid.toInt)
  override def getImage(jid:String,identity:String) = history.getMeTLHistory(jid).getImageByIdentity(identity).getOrElse(MeTLImage.empty)
  override def getResource(jid:String,url:String) = resourceProvider.getResource(url)
  override def getImage(identity:String):MeTLImage = MeTLImage.empty
  override def getResource(identifier:String):Array[Byte] = resourceProvider.getResource(identifier)
}
