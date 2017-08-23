package com.metl.persisted

import com.metl.data._
import com.metl.utils._

abstract class PersistenceInterface(config:ServerConfiguration) {
  def isReady:Boolean 
  def shutdown:Boolean
  def searchForConversation(query:String):List[Conversation]
  def conversationFor(slide:Int):Int
  def detailsOfConversation(conversationJid:Int):Conversation
  def getHistory(slideJid:String):History
  def getResource(identity:String):Array[Byte]
  def getResource(jid:String,identity:String):Array[Byte]
}
