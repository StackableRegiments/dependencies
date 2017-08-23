package com.metl.persisted

import com.metl.data._

class PersistedConversations(config:ServerConfiguration,dbInterface:PersistenceInterface) {
  def search(query:String) = dbInterface.searchForConversation(query)
  def conversationFor(slide:Int):Int = dbInterface.conversationFor(slide)
  def detailsOf(jid:Int) = dbInterface.detailsOfConversation(jid)
}
