package com.metl.model

object ServerConfiguration{
	val empty = EmptyBackendAdaptor
	private var serverConfigs:List[ServerConfiguration] = List(EmptyBackendAdaptor)
	def setServerConfigurations(sc:List[ServerConfiguration]) = serverConfigs = sc
	def setDefaultServerConfiguration(f:() => ServerConfiguration) = defaultConfigFunc = f
	def configForName(name:String) = serverConfigs.find(c => c.name == name).getOrElse(default)
	def configForHost(host:String) = serverConfigs.find(c => c.host == host).getOrElse(default)	
	private var defaultConfigFunc = () => serverConfigs(0)
	def default = {
		defaultConfigFunc()
		//configForName("transientMadam")
//		val host = tryo(xml.XML.loadString(Http.getClient.getAsString("http://metl.adm.monash.edu/server.xml")).text).openOr("reifier.adm.monash.edu.au")
//		configForHost(host)
	}

}

abstract class ServerConfiguration(incomingName:String,incomingHost:String) {
	val name = incomingName
	val host = incomingHost
	def getMessageBus(d:MessageBusDefinition):MessageBus
	def getHistory(jid:String):History
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
}

object EmptyBackendAdaptor extends ServerConfiguration("empty","empty"){
	val serializer = new PassthroughSerializer
	override def getMessageBus(d:MessageBusDefinition) = EmptyMessageBus
	override def getHistory(jid:String) = History.empty
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

object FrontendSerializationAdaptor extends ServerConfiguration("frontend","frontend"){
	val serializer = new GenericXmlSerializer("frontend")
	override def getMessageBus(d:MessageBusDefinition) = EmptyMessageBus
	override def getHistory(jid:String) = History.empty
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
