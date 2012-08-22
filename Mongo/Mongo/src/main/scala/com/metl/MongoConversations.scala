package com.metl.model

import net.liftweb._
import http._
import common._
import util._
import Helpers._
import collection._

class MongoConversations(configName:String,mongoHost:String,mongoPort:Int,mongoDB:String) extends ConversationRetriever(configName) {
	override def search(query:String) = List.empty[Conversation]
	override def conversationFor(slide:Int) = 0
	override def detailsOf(jid:Int) = Conversation.empty
	override def createConversation(title:String):Conversation = Conversation.empty
	override def updateConversation(jid:Int,conversation:Conversation) = conversation
}

