package com.metl.embedded

import com.metl.data._
import com.metl.utils._

import net.liftweb._
import http._
import common._
import util._
import Helpers._
import collection._

class EmbeddedConversations(configName:String,dbInterface:LocalEmbeddedInterface) extends ConversationRetriever(configName) {
	override def search(query:String) = List.empty[Conversation]
	override def conversationFor(slide:Int):Int = 0
	override def detailsOf(jid:Int) = Conversation.empty
	override def createConversation(title:String,author:String):Conversation = Conversation.empty
	override def deleteConversation(jid:String):Conversation = Conversation.empty	
	override def renameConversation(jid:String,newTitle:String):Conversation = Conversation.empty
	override def changePermissions(jid:String,newPermissions:Permissions):Conversation = Conversation.empty
	override def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = Conversation.empty
	override def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = Conversation.empty
	override def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = Conversation.empty
}
