package com.metl.h2

import com.metl.data._
import com.metl.utils._
import com.metl.persisted._
import com.metl.h2.dbformats._
import java.util.Date
import net.liftweb.mapper._
import net.liftweb.common._

class H2Interface(configName:String) extends PersistenceInterface{
	val serializer = new H2Serializer(configName)
	val config = ServerConfiguration.configForName(configName)

	Schemifier.schemify(true,Schemifier.infoF _, 
		List(
			H2Ink,
			H2Text,
			H2Image,
			H2DirtyInk,
			H2DirtyText,
			H2DirtyImage,
			H2MoveDelta,
			H2Quiz,
			H2QuizResponse,
			H2Command,
			H2Submission,
			H2Conversation,
			H2Resource
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
		stanza match {
			case s:MeTLInk => serializer.fromMeTLInk(s).room(jid).save	
			case s:MeTLText => serializer.fromMeTLText(s).room(jid).save
			case s:MeTLImage => serializer.fromMeTLImage(s).room(jid).save
			case s:MeTLDirtyInk => serializer.fromMeTLDirtyInk(s).room(jid).save
			case s:MeTLDirtyText => serializer.fromMeTLDirtyText(s).room(jid).save
			case s:MeTLDirtyImage => serializer.fromMeTLDirtyImage(s).room(jid).save
			case s:MeTLCommand => serializer.fromMeTLCommand(s).room(jid).save
			case s:MeTLQuiz => serializer.fromMeTLQuiz(s).room(jid).save
			case s:MeTLQuizResponse => serializer.fromMeTLQuizResponse(s).room(jid).save
			case s:MeTLSubmission => serializer.fromSubmission(s).room(jid).save
			case s:MeTLMoveDelta => serializer.fromMeTLMoveDelta(s).room(jid).save
			case _ => false
		}
	})

	def getHistory(jid:String):History = Stopwatch.time("H2Interface.getHistory",() => {
		val newHistory = History(jid)
		val inks = H2Ink.findAll(By(H2Ink.room,jid)).map(s => serializer.toMeTLInk(s))
		val texts = H2Text.findAll(By(H2Text.room,jid)).map(s => serializer.toMeTLText(s))
		val images = H2Image.findAll(By(H2Image.room,jid)).map(s => serializer.toMeTLImage(s))
		val dirtyInks = H2DirtyInk.findAll(By(H2DirtyInk.room,jid)).map(s => serializer.toMeTLDirtyInk(s))
		val dirtyTexts = H2DirtyText.findAll(By(H2DirtyText.room,jid)).map(s => serializer.toMeTLDirtyText(s))
		val dirtyImages = H2DirtyImage.findAll(By(H2DirtyImage.room,jid)).map(s => serializer.toMeTLDirtyImage(s))
		val moveDeltas = H2MoveDelta.findAll(By(H2MoveDelta.room,jid)).map(s => serializer.toMeTLMoveDelta(s))
		val submissions = H2Submission.findAll(By(H2Submission.room,jid)).map(s => serializer.toSubmission(s))
		val quizzes = H2Quiz.findAll(By(H2Quiz.room,jid)).map(s => serializer.toMeTLQuiz(s))
		val quizResponses = H2QuizResponse.findAll(By(H2QuizResponse.room,jid)).map(s => serializer.toMeTLQuizResponse(s))
		val commands = H2Command.findAll(By(H2Command.room,jid)).map(s => serializer.toMeTLCommand(s))
		(inks ::: texts ::: images ::: dirtyInks ::: dirtyTexts ::: dirtyImages ::: moveDeltas ::: quizzes ::: quizResponses ::: commands ::: submissions).foreach(s => newHistory.addStanza(s))
		newHistory
	})

	//conversations table
	protected val mbDef = new MessageBusDefinition("global","conversationUpdating",receiveConversationDetailsUpdated _)
	protected val conversationMessageBus = config.getMessageBus(mbDef)
	protected var conversationCache = scala.collection.mutable.Map(H2Conversation.findAll.map(c => (c.jid.is,serializer.toConversation(c))):_*)
	protected def updateConversation(c:Conversation):Boolean = {
		try {
			conversationCache.update(c.jid,c)
			conversationMessageBus.sendStanzaToRoom(MeTLCommand(config,c.author,new java.util.Date().getTime,"/UPDATE_CONVERSATION_DETAILS",List(c.jid.toString)))
			updateMaxJid
			serializer.fromConversation(c).save
			true
		} catch {
			case e:Throwable => {
				false
			}
		}
	}
	protected def updateMaxJid = maxJid = conversationCache.values.map(c => c.jid).max
	protected var maxJid = 0
	updateMaxJid
	protected def getNewJid = {
		val oldMax = maxJid
		maxJid += 1000
		oldMax 
	}
	protected def receiveConversationDetailsUpdated(m:MeTLStanza) = {
		m match {
			case c:MeTLCommand if c.command == "/UPDATE_CONVERSATION_DETAILS" && c.commandParameters.length == 1 => {
				try{
					println("metlCommand comprehended on the global thread: %s".format(c))
					val jidToUpdate = c.commandParameters(0).toInt
					conversationCache.update(jidToUpdate,detailsOfConversation(jidToUpdate))
					updateMaxJid
				} catch {
					case e:Throwable => println("exception while attempting to update conversation details")
				}
			}
			case _ => {}
		}
	}
	def searchForConversation(query:String):List[Conversation] = conversationCache.values.filter(c => c.title.toLowerCase.trim.contains(query.toLowerCase.trim) || c.author.toLowerCase.trim == query.toLowerCase.trim).toList
	def conversationFor(slide:Int):Int = (slide / 1000 ) * 1000
	def detailsOfConversation(jid:Int):Conversation = conversationCache(jid)
	def createConversation(title:String,author:String):Conversation = {
		val now = new Date()
		val newJid = getNewJid
		val details = Conversation(config,author,now.getTime,List(Slide(config,author,newJid + 1,0)),"unrestricted","",newJid,title,now.toString,Permissions.default(config))
		updateConversation(details)
		details	
	}
	protected def findAndModifyConversation(jidString:String,adjustment:Conversation => Conversation):Conversation  = Stopwatch.time("H2Interface.findAndModifyConversation", () => {
		try {
			val jid = jidString.toInt
			detailsOfConversation(jid) match {
				case c:Conversation if (c.jid.toString == jid) => {
					val updatedConv = adjustment(c)
					if (updateConversation(updatedConv)){
						updatedConv
					} else {
						Conversation.empty
					}
				}
				case other => other
			}
		} catch {
			case e:Throwable => {
				Conversation.empty
			}
		}
	})
	def deleteConversation(jid:String):Conversation = updateSubjectOfConversation(jid,"deleted")
	def renameConversation(jid:String,newTitle:String):Conversation = findAndModifyConversation(jid,c => c.rename(newTitle))
	def changePermissionsOfConversation(jid:String,newPermissions:Permissions):Conversation = findAndModifyConversation(jid,c => c.replacePermissions(newPermissions))
	def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = findAndModifyConversation(jid,c => c.replaceSubject(newSubject))
	def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = findAndModifyConversation(jid,c => c.addSlideAtIndex(index))
	def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = findAndModifyConversation(jid,c => c.replaceSlides(newSlides))

	//resources table
	def getResource(identity:String):Array[Byte] = Stopwatch.time("H2Interface.getResource", () => {
		H2Resource.find(By(H2Resource.url,identity)).map(r => r.bytes.is).openOr(Array.empty[Byte])
	})
	def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = Stopwatch.time("H2Interface.postResource", () => {
		val now = new Date().getTime.toString
		val possibleNewIdentity = "%s:%s:%s".format(jid,userProposedId,now)
		H2Resource.find(By(H2Resource.url,possibleNewIdentity)) match {
			case Full(r) => {
				val newUserProposedIdentity = "%s_%s".format(userProposedId,now) 
				postResource(jid,newUserProposedIdentity,data)
			}
			case _ => {
				H2Resource.create.url(possibleNewIdentity).bytes(data).save
				possibleNewIdentity
			}
		}
	})
}
