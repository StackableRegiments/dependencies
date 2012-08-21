package com.metl.model

import net.liftweb._
import http._
import common._
import util._
import Helpers._
import collection._

abstract class ConversationRetriever(configName:String) {
	lazy val config = ServerConfiguration.configForName(configName)
	def search(query:String):List[Conversation]
	def conversationFor(slide:Int):Int
	def detailsOf(jid:Int):Conversation 
	def createConversation(title:String):Conversation
	def updateConversation(jid:Int,conversation:Conversation):Conversation
}

object EmptyConversations extends ConversationRetriever("empty"){
	override def search(query:String) = List.empty[Conversation]
	override def conversationFor(slide:Int):Int = 0
	override def detailsOf(jid:Int) = Conversation.empty
	override def createConversation(title:String):Conversation = Conversation.empty
	override def updateConversation(jid:Int,conversation:Conversation) = conversation
}

