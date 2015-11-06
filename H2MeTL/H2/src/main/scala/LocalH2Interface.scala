package com.metl.h2

import com.metl.data._
import com.metl.utils._
import com.metl.persisted._
import com.metl.h2.dbformats._
import java.util.Date
import net.liftweb.mapper._
import net.liftweb.common._

import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}

class H2Interface(configName:String,filename:Option[String],onConversationDetailsUpdated:Conversation=>Unit) extends PersistenceInterface{
	lazy val serializer = new H2Serializer(configName)
	lazy val config = ServerConfiguration.configForName(configName)

  private val vendor = new StandardDBVendor("org.h2.Driver", filename.map(f => "jdbc:h2:%s;AUTO_SERVER=TRUE".format(f)).getOrElse("jdbc:h2:mem:%s".format(configName)),Empty,Empty)
  if (!DB.jndiJdbcConnAvailable_?) {
//			this right here?  This needs to be addressed.  Looks like I'm going to have to bring some lift libraries into this one.
//      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
  }

	def shutdown = vendor.closeAllConnections_!

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
      H2Attendance,
      H2File,
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
  val ATTENDANCES = "attendance"
	val COMMANDS = "command"
	
	//stanzas table
	def storeStanza[A <: MeTLStanza](jid:String,stanza:A):Option[A] = Stopwatch.time("H2Interface.storeStanza", () => {
		val transformedStanza:Option[_ <: H2MeTLStanza[_]] = stanza match {
      case s:MeTLStanza if s.isInstanceOf[Attendance] => Some(serializer.fromMeTLAttendance(s.asInstanceOf[Attendance]).room(jid))
      case s:Attendance => Some(serializer.fromMeTLAttendance(s).room(jid)) // for some reason, it just can't make this match
			case s:MeTLInk => Some(serializer.fromMeTLInk(s).room(jid))	
			case s:MeTLText => Some(serializer.fromMeTLText(s).room(jid))
			case s:MeTLImage => Some(serializer.fromMeTLImage(s).room(jid))
			case s:MeTLDirtyInk => Some(serializer.fromMeTLDirtyInk(s).room(jid))
			case s:MeTLDirtyText => Some(serializer.fromMeTLDirtyText(s).room(jid))
			case s:MeTLDirtyImage => Some(serializer.fromMeTLDirtyImage(s).room(jid))
			case s:MeTLCommand => Some(serializer.fromMeTLCommand(s).room(jid))
			case s:MeTLQuiz => Some(serializer.fromMeTLQuiz(s).room(jid))
			case s:MeTLQuizResponse => Some(serializer.fromMeTLQuizResponse(s).room(jid))
			case s:MeTLSubmission => Some(serializer.fromSubmission(s).room(jid))
			case s:MeTLMoveDelta => Some(serializer.fromMeTLMoveDelta(s).room(jid))
			case other => {
        println("didn't know how to transform stanza: %s".format(other))
        None
      }
		} 
		transformedStanza match {
			case Some(s) => {
        if (s.save){
          Some(serializer.toMeTLData(s)).flatMap(data => data match {
              case ms:A => Some(ms)
              case _ => None
            })
        } else {
          println("store in jid %s failed: %s".format(jid,stanza))
          None
        }
      }
			case _ => None
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
    val files = H2File.findAll(By(H2File.room,jid)).map(s => serializer.toMeTLFile(s))
    val attendances = H2Attendance.findAll(By(H2Attendance.location,jid)).map(s => serializer.toMeTLAttendance(s))
		val commands = H2Command.findAll(By(H2Command.room,jid)).map(s => serializer.toMeTLCommand(s))
		(inks ::: texts ::: images ::: dirtyInks ::: dirtyTexts ::: dirtyImages ::: moveDeltas ::: quizzes ::: quizResponses ::: commands ::: submissions ::: files ::: attendances).foreach(s => newHistory.addStanza(s))
		newHistory
	})

	//conversations table
	protected lazy val mbDef = new MessageBusDefinition("global","conversationUpdating",receiveConversationDetailsUpdated _)
	protected lazy val conversationMessageBus = config.getMessageBus(mbDef)
	protected lazy val conversationCache = scala.collection.mutable.Map(H2Conversation.findAll.map(c => (c.jid.get,serializer.toConversation(c))):_*)
	protected def updateConversation(c:Conversation):Boolean = {
		try {
			conversationCache.update(c.jid,c)
			updateMaxJid
			serializer.fromConversation(c).save
			conversationMessageBus.sendStanzaToRoom(MeTLCommand(config,c.author,new java.util.Date().getTime,"/UPDATE_CONVERSATION_DETAILS",List(c.jid.toString)))
			true
		} catch {
			case e:Throwable => {
				false
			}
		}
	}
	protected def updateMaxJid = maxJid = try {
		conversationCache.values.map(c => c.jid).max
	} catch {
		case _:Throwable => 0
	}
	protected var maxJid = 0
	protected def getNewJid = {
		if (maxJid == 0){
			updateMaxJid
		}
		val oldMax = maxJid
		maxJid += 1000
		maxJid 
	}
	protected def receiveConversationDetailsUpdated(m:MeTLStanza) = {
		m match {
			case c:MeTLCommand if c.command == "/UPDATE_CONVERSATION_DETAILS" && c.commandParameters.length == 1 => {
				try{
					val jidToUpdate = c.commandParameters(0).toInt
					val conversation = detailsOfConversation(jidToUpdate)
					conversationCache.update(conversation.jid,conversation)
					updateMaxJid
					onConversationDetailsUpdated(conversation)
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
				case c:Conversation if (c.jid == jid) => {
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
				println("failed to alter conversation, throwing: %s".format(e.getMessage))
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
  def updateConversation(jid:String,conversation:Conversation):Conversation = {
    if (jid == conversation.jid.toString){
      updateConversation(conversation)
      conversation
    } else {
      conversation
    }
  }

	//resources table
	def getResource(identity:String):Array[Byte] = Stopwatch.time("H2Interface.getResource", () => {
		H2Resource.find(By(H2Resource.url,identity)).map(r => {
			val b = r.bytes.get
			println("retrieved %s bytes for %s".format(b.length,identity))
			b
		}).openOr({
			println("failed to find bytes for %s".format(identity))
			Array.empty[Byte]
		})

	})
	def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = Stopwatch.time("H2Interface.postResource", () => {
		val now = new Date().getTime.toString
		val possibleNewIdentity = "%s:%s:%s".format(jid,userProposedId,now)
		H2Resource.find(By(H2Resource.url,possibleNewIdentity)) match {
			case Full(r) => {
				println("postResource: identityAlready exists for %s".format(userProposedId))
				val newUserProposedIdentity = "%s_%s".format(userProposedId,now) 
				postResource(jid,newUserProposedIdentity,data)
			}
			case _ => {
				H2Resource.create.url(possibleNewIdentity).bytes(data).room(jid).save
				println("postResource: saved %s bytes in %s at %s".format(data.length,jid,possibleNewIdentity))
				possibleNewIdentity
			}
		}
	})
}
