package com.metl.metl2011

import com.metl.data._
import com.metl.utils._
import java.io.ByteArrayInputStream

import net.liftweb.util._
import org.apache.commons.io.IOUtils
/*
class ConversationProviderCache(configName:String,conversationRetriever:ConversationRetriever,preCacheFunction:ConversationRetriever=>List[Conversation]) extends ConversationRetriever(configName) {
	val conversations = scala.collection.mutable.HashMap.empty[Int,Conversation]
	private def precacheConversations = Stopwatch.time("ConversationProviderCache.precacheConversations", () => {
		preCacheFunction(conversationRetriever).map(c => c.jid).map(conversationRetriever.detailsOf).filterNot(_ == Conversation.empty).foreach(c => conversations.put(c.jid,c))
	})
	override lazy val isReady:Boolean = {
		conversationRetriever.isReady
		precacheConversations
		true
	}
	override def search(query:String):List[Conversation] = Stopwatch.time("ConversationProviderCache.search", () => {
	  if (query == null || query.length == 0) 
			List.empty[Conversation]
    else {
      val lq = query.toLowerCase
      conversations.filter{
        case (jid,c)=>{
          c.title.toLowerCase.contains(lq) || c.author.toLowerCase == lq
        }
      }.map(_._2).toList
    }
	})
  override def detailsOf(conversationJid:Int) = Stopwatch.time("ConversationProviderCache.search", () => {
    try {
      conversations(conversationJid)
    }
    catch {
      case e:NoSuchElementException => conversationRetriever.detailsOf(conversationJid)
      case e:Throwable => Conversation.empty
    }
  })
  override def conversationFor(slide:Int):Int = Stopwatch.time("ConversationProviderCache.conversationFor",() => {
		conversations.find(c => c._2.slides.exists(s => s.id == slide)).map(c => c._2.jid.toInt).getOrElse(conversationRetriever.conversationFor(slide))
  })
  override def createConversation(title:String,author:String):Conversation = {
		val newC = conversationRetriever.createConversation(title,author)
		conversations.put(newC.jid,newC)
		newC
  }
  override def deleteConversation(jid:String):Conversation = {
		val newC = conversationRetriever.deleteConversation(jid)
		conversations.put(newC.jid,newC)
		newC
  }
  override def renameConversation(jid:String,newTitle:String):Conversation = {
		val newC = conversationRetriever.renameConversation(jid,newTitle)
		conversations.put(newC.jid,newC)
		newC
  }
  override def changePermissions(jid:String,newPermissions:Permissions):Conversation = {
		val newC = conversationRetriever.changePermissions(jid,newPermissions)
		conversations.put(newC.jid,newC)
		newC
  }
  override def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = {
		val newC = conversationRetriever.updateSubjectOfConversation(jid,newSubject)
		conversations.put(newC.jid,newC)
		newC
  }
  override def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = {
		val newC = conversationRetriever.addSlideAtIndexOfConversation(jid,index)
		conversations.put(newC.jid,newC)
		newC
  }
  override def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = {
		val newC = conversationRetriever.reorderSlidesOfConversation(jid,newSlides)
		conversations.put(newC.jid,newC)
		newC
  }
}

class ComposedMeTL2011CachedConversations(configName:String, http:SimpleAuthedHttpProvider, messageBusProvider:MessageBusProvider, onConversationDetailsUpdated:(Conversation) => Unit) extends ConversationProviderCache(configName,new MeTL2011Conversations(configName,"",http,messageBusProvider,(c) => {
		onConversationDetailsUpdated(c)
	}),(cr) => {
	try {
  	val serializer = new MeTL2011XmlSerializer(configName)
  	val rootAddress = "https://%s:1188".format(cr.config.host)
		val directoryUrl = "%s/Structure/".format(rootAddress)
		http.getClient.get(directoryUrl)
		val stream = new ByteArrayInputStream(http.getClient.getAsBytes("%s/all.zip".format(directoryUrl)))
		Unzipper.unzip(stream).map(x => {
			serializer.toConversation(x)
		}).toList
	} catch {
		case e:Throwable => List.empty[Conversation]
	}
})
*/
class MeTL2011CachedConversations(configName:String, http:SimpleAuthedHttpProvider, messageBusProvider:MessageBusProvider, onConversationDetailsUpdated:(Conversation) => Unit) extends MeTL2011Conversations(configName,"",http,messageBusProvider,onConversationDetailsUpdated) {
  val conversations = scala.collection.mutable.HashMap.empty[Int,Conversation]

  private def precacheConversations = {
		try {
			val directoryUrl = "%s/Structure/".format(rootAddress)
			http.getClient.get(directoryUrl)
			val stream = new ByteArrayInputStream(http.getClient.getAsBytes("%s/all.zip".format(directoryUrl)))
			val masterConversationList = Unzipper.unzip(stream).map(x => {
				serializer.toConversation(x)
			})
			masterConversationList.map(c=>c.jid).map(super.detailsOf).filterNot(_ == Conversation.empty).foreach(c=>conversations.put(c.jid,c))
		} catch {
			case e:Throwable => {}
		}
  }
  override lazy val isReady:Boolean = {
    precacheConversations
    true
  }

  override def search(query:String):List[Conversation] = Stopwatch.time("CachedConversations.search",() => {
    if(query == null || query.length == 0) List.empty[Conversation]
    else{
      val lq = query.toLowerCase
      conversations.filter{
        case (jid,c)=>{
          c.title.toLowerCase.contains(lq) || c.author.toLowerCase == lq
        }
      }.map(_._2).toList
    }
  })
  override def detailsOf(conversationJid:Int) = 
  {
    try {
      conversations(conversationJid)
    }
    catch {
      case e:NoSuchElementException => super.detailsOf(conversationJid)
      case e:Throwable => Conversation.empty
    }
  }
  override def pushConversationToServer(conversation:Conversation):Conversation = {
		//println("pushConversationToServer (cache): %s".format(conversation))
		val pushedConv = super.pushConversationToServer(conversation)
		conversations.put(pushedConv.jid,pushedConv)
		//println("pushConversationToServer (cache): %s".format(pushedConv))
		pushedConv
	}
	override def conversationFor(slide:Int):Int = Stopwatch.time("CachedConversations.conversationFor", () => {
		conversations.find{
			case (jid,c) => {
				c.slides.exists(s => s.id == slide)
			}
		}.map(ce => ce._1).getOrElse(-1)
	});
  override def receiveConversationDetailsUpdated(m:MeTLStanza):Option[Conversation] = {
		//println("receiveConversationDetailsUpdated (cache): %s".format(m))
    m match {
      case c:MeTLCommand if c.command == "/UPDATE_CONVERSATION_DETAILS" && c.commandParameters.length == 1 => {
        try{
          val conversation = super.detailsOf(c.commandParameters(0).toInt)
					//println("updating cache: %s".format(conversation))
					conversations.put(conversation.jid,conversation)
          onConversationDetailsUpdated(conversation)
          Some(conversation)
        } 
        catch {
          case e:Throwable =>{
						println("threw exception: "+e.getMessage)
            None
          }
        }
      }
      case _ => {
        None
      }
    }
	}
	override def notifyXmpp(remote:Conversation) = {
		conversations.put(remote.jid,remote)
		super.notifyXmpp(remote)
	}
}

class MeTL2011Conversations(configName:String, val searchBaseUrl:String, http:SimpleAuthedHttpProvider,messageBusProvider:MessageBusProvider,onConversationDetailsUpdated:(Conversation) => Unit) extends ConversationRetriever(configName,onConversationDetailsUpdated) {
  lazy val utils = new MeTL2011Utils(configName)
  lazy val serializer = new MeTL2011XmlSerializer(configName)
  lazy val rootAddress = "https://%s:1188".format(config.host)
  val mbDef = new MessageBusDefinition("global","conversationUpdating",receiveConversationDetailsUpdated _)
  val mb = messageBusProvider.getMessageBus(mbDef)

  def receiveConversationDetailsUpdated(m:MeTLStanza):Option[Conversation] = {
    m match {
      case c:MeTLCommand if c.command == "/UPDATE_CONVERSATION_DETAILS" && c.commandParameters.length == 1 => {
        try{
          val conversation = internalDetailsOf(c.commandParameters(0).toInt)
          onConversationDetailsUpdated(conversation)
          Some(conversation)
        } 
        catch {
          case e:Throwable =>{
						println("threw exception: "+e.getMessage)
            None
          }
        }
      }
      case _ => {
        None
      }
    }
  }

  override def search(query:String):List[Conversation] = Stopwatch.time("Conversations.search", () => {
    (scala.xml.XML.loadString(http.getClient.get(searchBaseUrl + "search?query=" + Helpers.urlEncode(query))) \\ "conversation").map(c => serializer.toConversation(c)).toList
  })
  override def conversationFor(slide:Int):Int = Stopwatch.time("Conversations.conversationFor",() => {
    config.name match {
      case "reifier" => ((slide / 1000) * 1000) + 400
      case "deified" => ((slide / 1000) * 1000) + 400
			//case "standalone" => ((slide /1000) * 1000) + 400
      case _ => (slide /1000) * 1000
    }
  })
  override def detailsOf(jid:Int):Conversation = Stopwatch.time("Conversations.detailsOf",() => internalDetailsOf(jid))
	private def internalDetailsOf(jid:Int):Conversation = Stopwatch.time("Conversations.internalDetailsOf", () => {
		try{
      (scala.xml.XML.loadString(http.getClient.get("https://"+config.host+":1188/Structure/"+utils.stem(jid.toString)+"/"+jid.toString+"/details.xml")) \\ "conversation").headOption.map(c => serializer.toConversation(c)).getOrElse(Conversation.empty)
    }
    catch{
      case e:Exception => {
        Conversation.empty
      }
    }
	})
  override def createConversation(title:String,author:String):Conversation = {
    val jid = getNewJid
    val now = new java.util.Date()
    val local = Conversation(config,author,now.getTime,List(Slide(config,author,jid + 1,0)),"unrestricted","",jid,title,now.toString,Permissions.default(config))
    pushConversationToServer(local)
  }
  override def deleteConversation(jid:String):Conversation = {
    val conv = detailsOf(jid.toInt)
    val now = new java.util.Date()
    val local = Conversation(config,conv.author,now.getTime,conv.slides,"deleted",conv.tag,conv.jid,conv.title,conv.created,conv.permissions)
    pushConversationToServer(local)
  }
  override def renameConversation(jid:String,newTitle:String):Conversation = {
    val conv = detailsOf(jid.toInt)
    val now = new java.util.Date()
    val local = Conversation(config,conv.author,now.getTime,conv.slides,conv.subject,conv.tag,conv.jid,newTitle,conv.created,conv.permissions)
    pushConversationToServer(local)
  }
  override def changePermissions(jid:String,newPermissions:Permissions):Conversation = {
    val conv = detailsOf(jid.toInt)
    val now = new java.util.Date()
    val local = Conversation(config,conv.author,now.getTime,conv.slides,conv.subject,conv.tag,conv.jid,conv.title,conv.created,newPermissions)
    pushConversationToServer(local)
  }
  override def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = {
    val conv = detailsOf(jid.toInt)
    val now = new java.util.Date()
    val local = Conversation(config,conv.author,now.getTime,conv.slides,newSubject,conv.tag,conv.jid,conv.title,conv.created,conv.permissions)
    pushConversationToServer(local)
  }
  override def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = {
    val conv = detailsOf(jid.toInt)
    val slides = conv.slides
    val currentMaxJid = slides.map(s => s.id) match {
			case l:List[Int] if (l.length > 0) => l.max
			case _ => jid.toInt
		}
    val newSlides = slides.map(s => {
      val newIndex = s.index match {
        case i:Int if (i < index) => i
        case i:Int => i + 1
      }
      Slide(config,s.author,s.id,newIndex,s.defaultHeight,s.defaultWidth,s.exposed,s.slideType)
    })
    val newSlide = Slide(config,conv.author,currentMaxJid + 1, index)
    val now = new java.util.Date()
    val local = Conversation(config,conv.author,now.getTime,newSlide :: newSlides,conv.subject,conv.tag,conv.jid,conv.title,conv.created,conv.permissions)
    pushConversationToServer(local)
  }
  override def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = {
    val conv = detailsOf(jid.toInt)
    val now = new java.util.Date()
    val local = Conversation(config,conv.author,now.getTime,newSlides,conv.subject,conv.tag,conv.jid,conv.title,conv.created,conv.permissions)
    pushConversationToServer(local)
  }
  protected def pushConversationToServer(conversation:Conversation):Conversation = {
		//println("pushConversationToServer (proposed): %s".format(conversation))
    val jid = conversation.jid
    val bytes = serializer.fromConversation(conversation).toString.getBytes("UTF-8")
    val url = "%s/upload_nested.yaws?overwrite=true&path=%s&filename=details.xml".format(rootAddress,Helpers.urlEncode("Structure/%s/%s".format(utils.stem(jid.toString),jid.toString)))
    http.getClient.postBytes(url,bytes)
    val remote = internalDetailsOf(jid)
    notifyXmpp(remote)
		//println("pushConversationToServer (confirmed): %s".format(remote))
    remote
  }
  protected def notifyXmpp(newConversation:Conversation) = {
    mb.sendStanzaToRoom(MeTLCommand(config,newConversation.author,new java.util.Date().getTime,"/UPDATE_CONVERSATION_DETAILS",List(newConversation.jid.toString)))
  }
  private def getNewJid:Int = http.getClient.get("https://"+config.host+":1188/primarykey.yaws").trim.toInt
}
