package com.metl.metl2011

import com.metl.data._
import com.metl.utils._
import scala.xml._

object MeTL2011ServerConfiguration{
  def initialize = List(
    MeTL2011BackendAdaptorConfigurator,
    TransientMeTL2011BackendAdaptorConfigurator
  ).foreach(sc => ServerConfiguration.addServerConfigurator(sc))
}

class MeTL2011BackendAdaptor(name:String,hostname:String,xmppDomainName:String,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentials:Tuple2[String,String],conversationBusCredentials:Tuple2[String,String],httpCredentials:Tuple2[String,String]) extends ServerConfiguration(name,hostname,onConversationDetailsUpdated){
  protected val http = new SimpleAuthedHttpProvider(httpCredentials._1,httpCredentials._2)
  protected lazy val history = new MeTL2011History(name,http)
  protected lazy val messageBusProvider = new PooledXmppProvider(name,hostname,messageBusCredentials._1,messageBusCredentials._2,xmppDomainName)
  protected lazy val conversationsMessageBusProvider = new XmppProvider(name,hostname,conversationBusCredentials._1,conversationBusCredentials._2,xmppDomainName)
  protected val conversations = new MeTL2011CachedConversations(name,http,conversationsMessageBusProvider,onConversationDetailsUpdated)
  lazy val serializer = new MeTL2011XmlSerializer(name)
  override def isReady = {
    conversations.isReady
  }
  protected val resourceProvider = new MeTL2011Resources(name,http)
  override def getMessageBus(d:MessageBusDefinition) = messageBusProvider.getMessageBus(d)
  override def getHistory(jid:String) = history.getMeTLHistory(jid)
  override def getConversationForSlide(slideJid:String) = conversations.conversationFor(slideJid.toInt).toString
  override def searchForConversation(query:String) = conversations.search(query)
  override def detailsOfConversation(jid:String) = conversations.detailsOf(jid.toInt)
  override def createConversation(title:String,author:String) = conversations.createConversation(title,author)
  override def deleteConversation(jid:String):Conversation = conversations.deleteConversation(jid)
  override def renameConversation(jid:String,newTitle:String):Conversation = conversations.renameConversation(jid,newTitle)
  override def changePermissions(jid:String,newPermissions:Permissions):Conversation = conversations.changePermissions(jid,newPermissions)
  override def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = conversations.updateSubjectOfConversation(jid,newSubject)
  override def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = conversations.addSlideAtIndexOfConversation(jid,index)
  override def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = conversations.reorderSlidesOfConversation(jid,newSlides)
  override def getImage(jid:String,identity:String) = history.getMeTLHistory(jid).getImageByIdentity(identity).getOrElse(MeTLImage.empty)
  override def getResource(url:String) = http.getClient.getAsBytes(url)
  override def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = resourceProvider.postResource(jid,userProposedId,data)
}

object MeTL2011BackendAdaptorConfigurator extends ServerConfigurator{
  override def matchFunction(e:Node) = (e \\ "type").text == "MeTL2011"
  override def interpret(e:Node,onConversationDetailsUpdated:Conversation=>Unit) = {
    val name = (e \\ "name").text
    val host = (e \\ "host").text
    val xmppDomainName = (e \\ "xmppDomainName").text
    val httpUsername = (e \\ "httpUsername").text
    val httpPassword = (e \\ "httpPassword").text
    val messageBusUsernamePrefix = (e \\ "messageBusUsernamePrefix").text
    val messageBusPassword = (e \\ "messageBusPassword").text
    val conversationListenerUsernamePrefix = (e \\ "conversationListenerUsernamePrefix").text
    val conversationListenerPassword = (e \\ "conversationListenerPassword").text
    xmppDomainName match {
      case s:String if s.length > 0 => Some(new MeTL2011BackendAdaptor(name,host,s,onConversationDetailsUpdated,(messageBusUsernamePrefix,messageBusPassword),(conversationListenerUsernamePrefix,conversationListenerPassword),(httpUsername,httpPassword)))
      case _ => Some(new MeTL2011BackendAdaptor(name,host,host,onConversationDetailsUpdated,(messageBusUsernamePrefix,messageBusPassword),(conversationListenerUsernamePrefix,conversationListenerPassword),(httpUsername,httpPassword)))
    }
  }
}

class TransientMeTL2011BackendAdaptor(name:String,hostname:String,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentials:Tuple2[String,String],conversationBusCredentials:Tuple2[String,String],httpCredentials:Tuple2[String,String]) extends ServerConfiguration(name,hostname,onConversationDetailsUpdated){
  protected val http = new SimpleAuthedHttpProvider(httpCredentials._1,httpCredentials._2)
  protected val history = new MeTL2011History(name,http)
  protected val messageBusProvider = new LoopbackMessageBusProvider
  protected val conversations = new MeTL2011CachedConversations(name,http,messageBusProvider,onConversationDetailsUpdated)
  val serializer = new MeTL2011XmlSerializer(name)
  override def getMessageBus(d:MessageBusDefinition) = messageBusProvider.getMessageBus(d)
  override def getHistory(jid:String) = history.getMeTLHistory(jid)
  override def getConversationForSlide(slideJid:String) = conversations.conversationFor(slideJid.toInt).toString
  override def searchForConversation(query:String) = conversations.search(query)
  override def detailsOfConversation(jid:String) = conversations.detailsOf(jid.toInt)
  override def createConversation(title:String,author:String) = Conversation.empty
  override def deleteConversation(jid:String):Conversation = Conversation.empty
  override def renameConversation(jid:String,newTitle:String):Conversation = Conversation.empty
  override def changePermissions(jid:String,newPermissions:Permissions):Conversation = Conversation.empty
  override def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = Conversation.empty
  override def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = Conversation.empty
  override def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = Conversation.empty
  override def getImage(jid:String,identity:String) = history.getMeTLHistory(jid).getImageByIdentity(identity).getOrElse(MeTLImage.empty)
  override def getResource(url:String) = http.getClient.getAsBytes(url)
  override def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = "not yet implemented"
}

object TransientMeTL2011BackendAdaptorConfigurator extends ServerConfigurator{
  override def matchFunction(e:Node) = (e \\ "type").text == "TransientMeTL2011"
  override def interpret(e:Node,onConversationDetailsUpdated:Conversation=>Unit) = {
    val name = (e \\ "name").text
    val host = (e \\ "host").text
    val httpUsername = (e \\ "httpUsername").text
    val httpPassword = (e \\ "httpPassword").text
    val messageBusUsernamePrefix = (e \\ "messageBusUsernamePrefix").text
    val messageBusPassword = (e \\ "messageBusPassword").text
    val conversationListenerUsernamePrefix = (e \\ "conversationListenerUsernamePrefix").text
    val conversationListenerPassword = (e \\ "conversationListenerPassword").text
    Some(new TransientMeTL2011BackendAdaptor(name,host,onConversationDetailsUpdated,(messageBusUsernamePrefix,messageBusPassword),(conversationListenerUsernamePrefix,conversationListenerPassword),(httpUsername,httpPassword)))
  }
}
