package com.metl.h2

import com.metl.data._
import com.metl.utils._
import com.metl.persisted._

import net.liftweb.mapper._

class H2Interface(configName:String) extends PersistenceInterface{
	val serializer = new H2Serializer(configName)

	Schemifier.schemify(true,Schemifier.infoF _, 
		List(
			dbformats.Ink,
			dbformats.Text,
			dbformats.Image,
			dbformats.DirtyInk,
			dbformats.DirtyText,
			dbformats.DirtyImage,
			dbformats.MoveDelta,
			dbformats.Quiz,
			dbformats.QuizResponse,
			dbformats.Command,
			dbformats.Submission,
			dbformats.Conversation,
			dbformats.Resource
		):_*
	)

	type H2Object = Object
	val RESOURCES = "resource"
	val CONVERSATIONS = "conversation"
	val INKS = "ink"
	val TEXTS = "text"
	val IMAGES = "image"
	val DIRTYINKS = "dirtyInk"
	val DIRTYTEXTS = "dirtyText"
	val DIRTYIMAGES = "dirtyImage"
	val MOVEDELTAS = "moveDelta"
	val SUBMISSIONS = "submission"
	val QUIZZES = "quiz"
	val QUIZRESPONSES = "quizResponse"
	val COMMANDS = "command"
	
	//stanzas table
	def storeStanza(jid:String,stanza:MeTLStanza):Boolean = Stopwatch.time("H2Interface.storeStanza", () => {
/*
		stanza match {
			case s:MeTLInk => store(INKS,serializer.fromInk(s))
			case s:MeTLText => store(TEXTS,serializer.fromText(s))
			case s:MeTLImage => store(IMAGES,serializer.fromImage(s))
			case s:MeTLDirtyInk => store(DIRTYINKS,serializer.fromDirtyInk(s))
			case s:MeTLDirtyImage => store(DIRTYIMAGES,serializer.fromDirtyImage(s))
			case s:MeTLDirtyText => store(DIRTYTEXTS,serializer.fromDirtyText(s))
			case s:MeTLMoveDelta => store(MOVEDELTAS,serializer.fromMoveDelta(s))
			case s:MeTLSubmission => store(SUBMISSIONS,serializer.fromSubmission(s))
			case s:MeTLCommand => store(COMMANDS,serializer.fromCommand(s))
			case s:MeTLQuiz => store(QUIZZES,serializer.fromMeTLQuiz(s))
			case s:MeTLQuizOption => store(COMMANDS,serializer.fromMeTLQuizOption(s))
		}
*/
		false
	})

	def getHistory(jid:String):History = Stopwatch.time("H2Interface.getHistory",() => {
/*
		val newHistory = History(jid)
		List(INKS,TEXTS,IMAGES,DIRTYINKS,DIRTYIMAGES,DIRTYTEXTS,MOVEDELTAS,SUBMISSIONS,COMMANDS,QUIZZES,QUIZRESPONSES).foreach(l => {
			retrieve(l,"SELECT * WHERE JID = %s".format(jid)).map(s => {
				newHistory.addStanza(s)
			})
		})	
		newHistory
*/
		History.empty
	})

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

	//resources table will have to be: STRING(jid),STRING(id),BINARY(data)

	//resources table
	def getResource(identity:String):Array[Byte] = Stopwatch.time("H2Interface.getResource", () => {
	//	retrieve(RESOURCES, "SELECT * FROM %s WHERE IDENTITY = %s LIMIT 1".format(RESOURCES,identity))
		Array.empty[Byte]
	})
	def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = Stopwatch.time("H2Interface.postResource", () => {
//		store(RESOURCES,null)
		""
	})

	protected def store(table:String,h2Storeable:H2Object):Boolean = Stopwatch.time("H2Interface.store", () => {
		false
	})
	protected def retrieve(table:String,query:String):List[H2Object] = Stopwatch.time("H2Interface.retrieve", () => {
		List.empty[H2Object]
	})
}
