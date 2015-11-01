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

class MeTL2011BackendAdaptor(name:String,hostname:String,xmppDomainName:String,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentialFunc:()=>Tuple2[String,String],conversationBusCredentialFunc:()=>Tuple2[String,String],httpCredentialFunc:()=>Tuple2[String,String]) extends ServerConfiguration(name,hostname,onConversationDetailsUpdated){
  protected val http:HttpProvider = new DynamicallyAuthedHttpProvider(httpCredentialFunc)
  protected lazy val history = new MeTL2011History(name,http)
  protected lazy val messageBusProvider = new PooledXmppProvider(name,hostname,messageBusCredentialFunc,xmppDomainName)
  protected lazy val conversationsMessageBusProvider = new XmppProvider(name,hostname,conversationBusCredentialFunc,xmppDomainName)
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
  override def interpret(e:Node,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentailsFunc:()=>Tuple2[String,String],conversationListenerCredentialsFunc:()=>Tuple2[String,String],httpCredentialsFunc:()=>Tuple2[String,String]) = {
    for (
      name <- (e \\ "name").headOption.map(_.text)
      host <- (e \\ "host").headOption.map(_.text)
      xmppDomainName = (e \\ "xmppDomainName").headOption.map(_.text).filter(_.length > 0)
      httpUsername <- (e \\ "httpUsername").headOption.map(_.text)
      httpPassword <- (e \\ "httpPassword").headOption.map(_.text)
      messageBusUsernamePrefix <- (e \\ "messageBusUsernamePrefix").headOption.map(_.text)
      messageBusPassword <- (e \\ "messageBusPassword").headOption.map(_.text)
      conversationListenerUsernamePrefix <- (e \\ "conversationListenerUsernamePrefix").headOption.map(_.text)
      conversationListenerPassword <- (e \\ "conversationListenerPassword").headOption.map(_.text)
    ) yeild {
      Some(new MeTL2011BackendAdaptor(name,host,xmppDomainName.getOrElse(host),onConversationDetailsUpdated,() => (messageBusUsernamePrefix,messageBusPassword),() => (conversationListenerUsernamePrefix,conversationListenerPassword),() => (httpUsername,httpPassword)))
    }
  }
}

object MeTL2015BackendAdaptorConfigurator extends ServerConfigurator{
  override def matchFunction(e:Node) = (e \\ "type").text == "MeTL2015"
  override def interpret(e:Node,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentailsFunc:()=>Tuple2[String,String],conversationListenerCredentialsFunc:()=>Tuple2[String,String],httpCredentialsFunc:()=>Tuple2[String,String]) = {
    for (
      name <- (e \\ "name").headOption.map(_.text)
      host <- (e \\ "host").headOption.map(_.text)
      xmppDomainName = (e \\ "xmppDomainName").headOption.map(_.text).filter(_.length > 0)
    ) yield {
      Some(new MeTL2011BackendAdaptor(name,host,xmppDomainName.getOrElse(host),onConversationDetailsUpdated,messageBusCredentialsFunc,conversationListenerCredentialsFunc,httpCredentialsFunc))
  }
}


  class TransientMeTL2011BackendAdaptor(name:String,hostname:String,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentials:() => Tuple2[String,String],conversationBusCredentials:Tuple2[String,String],httpCredentials:Tuple2[String,String]) extends ServerConfiguration(name,hostname,onConversationDetailsUpdated){
  protected val http = new DynamicallyAuthedHttpProvider(httpCredentials._1,httpCredentials._2)
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
  override def matchFunction(e:Node) = (e \\ "type").text == "MeTL2011"
  override def interpret(e:Node,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentailsFunc:()=>Tuple2[String,String],conversationListenerCredentialsFunc:()=>Tuple2[String,String],httpCredentialsFunc:()=>Tuple2[String,String]) = {
    for (
      name <- (e \\ "name").headOption.map(_.text)
      host <- (e \\ "host").headOption.map(_.text)
      xmppDomainName = (e \\ "xmppDomainName").headOption.map(_.text).filter(_.length > 0)
      httpUsername <- (e \\ "httpUsername").headOption.map(_.text)
      httpPassword <- (e \\ "httpPassword").headOption.map(_.text)
      messageBusUsernamePrefix <- (e \\ "messageBusUsernamePrefix").headOption.map(_.text)
      messageBusPassword <- (e \\ "messageBusPassword").headOption.map(_.text)
      conversationListenerUsernamePrefix <- (e \\ "conversationListenerUsernamePrefix").headOption.map(_.text)
      conversationListenerPassword <- (e \\ "conversationListenerPassword").headOption.map(_.text)
    ) yeild {
      Some(new TransientMeTL2011BackendAdaptor(name,host,xmppDomainName.getOrElse(host),onConversationDetailsUpdated,() => (messageBusUsernamePrefix,messageBusPassword),() => (conversationListenerUsernamePrefix,conversationListenerPassword),() => (httpUsername,httpPassword)))
    }
  }
}

object TransientMeTL2015BackendAdaptorConfigurator extends ServerConfigurator{
  override def matchFunction(e:Node) = (e \\ "type").text == "MeTL2015"
  override def interpret(e:Node,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentailsFunc:()=>Tuple2[String,String],conversationListenerCredentialsFunc:()=>Tuple2[String,String],httpCredentialsFunc:()=>Tuple2[String,String]) = {
    for (
      name <- (e \\ "name").headOption.map(_.text)
      host <- (e \\ "host").headOption.map(_.text)
      xmppDomainName = (e \\ "xmppDomainName").headOption.map(_.text).filter(_.length > 0)
    ) yield {
      Some(new MeTL2011BackendAdaptor(name,host,xmppDomainName.getOrElse(host),onConversationDetailsUpdated,messageBusCredentialsFunc,conversationListenerCredentialsFunc,httpCredentialsFunc))
  }
}


