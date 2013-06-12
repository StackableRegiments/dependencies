package com.metl.data

import com.metl.utils._
import scala.xml._

object ServerConfiguration{
  val empty = EmptyBackendAdaptor
  private var serverConfigs:List[ServerConfiguration] = List(EmptyBackendAdaptor)
  def setServerConfigurations(sc:List[ServerConfiguration]) = serverConfigs = sc
  def getServerConfigurations = serverConfigs
  def setDefaultServerConfiguration(f:() => ServerConfiguration) = defaultConfigFunc = f
  def addServerConfiguration(sc:ServerConfiguration) = serverConfigs = serverConfigs ::: List(sc)
  def configForName(name:String) = serverConfigs.find(c => c.name == name).getOrElse(default)
  def configForHost(host:String) = serverConfigs.find(c => c.host == host).getOrElse(default)
  private var defaultConfigFunc = () => serverConfigs(0)
  def default = {
    defaultConfigFunc()
  }
  protected var serverConfigurators:List[ServerConfigurator] = List(
    EmptyBackendAdaptorConfigurator,
    FrontendSerializationAdaptorConfigurator
  )
  def addServerConfigurator(sc:ServerConfigurator) = serverConfigurators = serverConfigurators ::: List(sc)
  def loadServerConfigsFromFile(path:String,onConversationDetailsUpdated:Conversation=>Unit) = {
    val xml = XML.load(path)
      (xml \\ "server").foreach(sc => interpret(sc,onConversationDetailsUpdated))
      (xml \\ "defaultServerConfiguration").text match {
        case s:String if (s.length > 0) => defaultConfigFunc = () => configForName(s)
        case _ => {}
      }
  }
  protected def interpret(n:Node,onConversationDetailsUpdated:Conversation=>Unit) = serverConfigurators.filter(sc => sc.matchFunction(n)).map(sc => sc.interpret(n,onConversationDetailsUpdated).map(s => addServerConfiguration(s)))
}

class ServerConfigurator{
  def matchFunction(e:Node) = false
  def interpret(e:Node,onConversationDetailsUpdated:Conversation=>Unit):Option[ServerConfiguration] = None
}

abstract class ServerConfiguration(incomingName:String,incomingHost:String,onConversationDetailsUpdated:Conversation=>Unit) {
  val name = incomingName
  val host = incomingHost
  def getMessageBus(d:MessageBusDefinition):MessageBus
  def getHistory(jid:String):History
  def getConversationForSlide(slideJid:String):String
  def searchForConversation(query:String):List[Conversation]
  def detailsOfConversation(jid:String):Conversation
  def createConversation(title:String,author:String):Conversation
  def deleteConversation(jid:String):Conversation
  def renameConversation(jid:String,newTitle:String):Conversation
  def changePermissions(jid:String,newPermissions:Permissions):Conversation
  def updateSubjectOfConversation(jid:String,newSubject:String):Conversation
  def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation
  def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation
  def getImage(jid:String,identity:String):MeTLImage
  def getResource(url:String):Array[Byte]
  def postResource(jid:String,userProposedId:String,data:Array[Byte]):String
  //shutdown is a function to be called when the serverConfiguration is to be disposed
  def shutdown:Unit = {}
  def isReady:Boolean = true
}

object EmptyBackendAdaptor extends ServerConfiguration("empty","empty",(c)=>{}){
  val serializer = new PassthroughSerializer
  override def getMessageBus(d:MessageBusDefinition) = EmptyMessageBus
  override def getHistory(jid:String) = History.empty
  override def getConversationForSlide(slideJid:String):String = ""
  override def searchForConversation(query:String) = List.empty[Conversation]
  override def detailsOfConversation(jid:String) = Conversation.empty
  override def createConversation(title:String,author:String) = Conversation.empty
  override def deleteConversation(jid:String):Conversation = Conversation.empty
  override def renameConversation(jid:String,newTitle:String):Conversation = Conversation.empty
  override def changePermissions(jid:String,newPermissions:Permissions):Conversation = Conversation.empty
  override def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = Conversation.empty
  override def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = Conversation.empty
  override def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = Conversation.empty
  override def getImage(jid:String,identity:String) = MeTLImage.empty
  override def getResource(url:String) = Array.empty[Byte]
  override def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = ""
}

object EmptyBackendAdaptorConfigurator extends ServerConfigurator{
  override def matchFunction(e:Node) = (e \\ "type").text == "empty"
  override def interpret(e:Node,o:Conversation=>Unit) = Some(EmptyBackendAdaptor)
}

object FrontendSerializationAdaptor extends ServerConfiguration("frontend","frontend",(c)=>{}){
  val serializer = new GenericXmlSerializer("frontend")
  override def getMessageBus(d:MessageBusDefinition) = EmptyMessageBus
  override def getHistory(jid:String) = History.empty
  override def getConversationForSlide(slideJid:String):String = ""
  override def searchForConversation(query:String) = List.empty[Conversation]
  override def detailsOfConversation(jid:String) = Conversation.empty
  override def createConversation(title:String,author:String) = Conversation.empty
  override def deleteConversation(jid:String):Conversation = Conversation.empty
  override def renameConversation(jid:String,newTitle:String):Conversation = Conversation.empty
  override def changePermissions(jid:String,newPermissions:Permissions):Conversation = Conversation.empty
  override def updateSubjectOfConversation(jid:String,newSubject:String):Conversation = Conversation.empty
  override def addSlideAtIndexOfConversation(jid:String,index:Int):Conversation = Conversation.empty
  override def reorderSlidesOfConversation(jid:String,newSlides:List[Slide]):Conversation = Conversation.empty
  override def getImage(jid:String,identity:String) = MeTLImage.empty
  override def getResource(url:String) = Array.empty[Byte]
  override def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = ""
}

object FrontendSerializationAdaptorConfigurator extends ServerConfigurator{
  override def matchFunction(e:Node) = (e \\ "type").text == "frontend"
  override def interpret(e:Node,o:Conversation=>Unit) = Some(FrontendSerializationAdaptor)
}
