package com.metl.h2

import com.metl.data._
import com.metl.utils._
import com.metl.persisted._

class H2Interface(configName:String,serializer:Serializer) extends PersistenceInterface{
	//stanzas table
	def storeStanza(jid:String,stanza:MeTLStanza):Boolean = true
	def getHistory(jid:String):History = History.empty

	//conversations table
	def searchForConversation(query:String):List[Conversation] = List.empty[Conversation]
	def conversationFor(slide:Int):Int = 0
	def detailsOfConversation(jid:Int):Conversation = Conversation.empty
	def createConversation(title:String,author:String):Conversation = Conversation.empty
	def deleteConversation(jid:String):Conversation = Conversation.empty
	def renameConversation(jid:String,newTitle:String):Conversation = Conversation.empty
	def changePermissionsOfConversation(jid:String,newPermissions:Permissions):Conversation = Conversation.empty
	def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = Conversation.empty
	def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = Conversation.empty
	def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = Conversation.empty

	//resources table
	def getResource(identity:String):Array[Byte] = Array.empty[Byte]
	def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = ""
}
