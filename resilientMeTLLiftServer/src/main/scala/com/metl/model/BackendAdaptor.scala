package com.metl.model

import com.metl.cas._
import com.metl.data._
import com.metl.metl2011._
import com.metl.utils._

import _root_.net.liftweb.util._
import Helpers._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.rest._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import com.metl.snippet._
import com.metl.view._
import com.metl.cas._
import com.metl.auth._
import com.metl.h2._

object MeTLXConfiguration {
  protected var configs:Map[String,Tuple2[ServerConfiguration,RoomProvider]] = Map.empty[String,Tuple2[ServerConfiguration,RoomProvider]]
	def setupForStandalone = {
		 val auth = new CASAuthenticator("metlx",() => Globals.casState.authenticated, (cs:com.metl.cas.CASStateData) => {
      println("loginHandler")
      Globals.casState(cs)
      Globals.currentUser(cs.username)
    })
		MeTL2011ServerConfiguration.initialize
		ServerConfiguration.loadServerConfigsFromFile("servers.standalone.xml")
		val servers = ServerConfiguration.getServerConfigurations
    configs = Map(servers.map(c => (c.name,(c,new HistoryCachingRoomProvider(c.name)))):_*)
    Globals.isDevMode match {
      case false => CASAuthentication.attachCASAuthenticator(auth)
      case _ => {}
    }
	}
  def setupForExternal = {
    val auth = new OpenIdAuthenticator(()=>Globals.casState.authenticated,(cs:com.metl.cas.CASStateData) => {
      println("openId loginHandler")
      Globals.casState(cs)
      Globals.currentUser(cs.username)
    })
		LocalH2ServerConfiguration.initialize
		ServerConfiguration.loadServerConfigsFromFile("servers.external.xml")
		val servers = ServerConfiguration.getServerConfigurations
    configs = Map(servers.map(c => (c.name,(c,new HistoryCachingRoomProvider(c.name)))):_*)
    Globals.isDevMode match {
      case false => OpenIdAuthenticator.attachOpenIdAuthenticator(auth)
      case _ => {}
    }
  }
  def setupForMonash = {
    val auth = new CASAuthenticator("metlx",() => Globals.casState.authenticated, (cs:com.metl.cas.CASStateData) => {
      println("loginHandler")
      Globals.casState(cs)
      Globals.currentUser(cs.username)
    })
		MeTL2011ServerConfiguration.initialize
		ServerConfiguration.loadServerConfigsFromFile("servers.monash.xml")
		val servers = ServerConfiguration.getServerConfigurations
    configs = Map(servers.map(c => (c.name,(c,new HistoryCachingRoomProvider(c.name)))):_*)
    Globals.isDevMode match {
      case false => CASAuthentication.attachCASAuthenticator(auth)
      case _ => {}
    }
  }
  def initializeSystem = {
    Props.mode match {
      case Props.RunModes.Production => Globals.isDevMode = false
      case _=> Globals.isDevMode = true
    }
    val prop = System.getProperty("metl.backend")
    println("startupParams: "+prop)
    prop.toLowerCase match {
      case "monash" => setupForMonash
			case "standalone" => setupForStandalone
      case _ => setupForExternal
    }
    // Setup RESTful endpoints (these are in view/Endpoints.scala)
    LiftRules.statelessDispatchTable.prepend(MeTLRestHelper)
    LiftRules.dispatch.append(MeTLStatefulRestHelper)
    LiftRules.statelessDispatchTable.prepend(WebMeTLRestHelper)
    LiftRules.dispatch.append(WebMeTLStatefulRestHelper)
		configs.values.foreach(c => {
			println("%s is now ready for use (%s)".format(c._1.name,c._1.isReady))
		})
    configs.values.foreach(c => LiftRules.unloadHooks.append(c._1.shutdown _))
  }
  def getRoom(jid:String,configName:String) = {
    configs(configName)._2.get(jid)
  }
}

class TransientLoopbackAdaptor(configName:String) extends ServerConfiguration(configName,"no_host"){
  val serializer = new PassthroughSerializer
  val messageBusProvider = new LoopbackMessageBusProvider
  override def getMessageBus(d:MessageBusDefinition) = messageBusProvider.getMessageBus(d)
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
