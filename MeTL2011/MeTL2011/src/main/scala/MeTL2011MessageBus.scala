package com.metl.metl2011

import com.metl.data._
import com.metl.utils._
import com.metl.xmpp._

import java.util.Random
import net.liftweb.util.Helpers._
import scala.xml._
import java.util.Date
import scala.collection.mutable.HashMap

class XmppProvider(configName:String,hostname:String,username:String,password:String,domainName:String) extends OneBusPerRoomMessageBusProvider{
  override def createNewMessageBus(d:MessageBusDefinition) = Stopwatch.time("XmppProvider.createNewMessageBus", () => {
    new XmppMessageBus(configName,hostname,username + new java.util.Date().getTime.toString,password,domainName,d,this)
  })
	def getHostname = hostname
	def getUsername = username
	def getPassword = password
	def getDomainName = domainName
}

class PooledXmppProvider(configName:String,hostname:String,username:String,password:String,domainName:String) extends OneBusPerRoomMessageBusProvider{
	private val connMgr = new XmppConnProvider(configName,hostname,username,password,domainName)
  override def createNewMessageBus(d:MessageBusDefinition) = Stopwatch.time("PooledXmppProvider.createNewMessageBus", () => {
		val conn = connMgr.getConn
    val bus = new XmppSharedConnMessageBus(configName,hostname,username + new java.util.Date().getTime.toString,password,domainName,d,this)
		bus.addConn(conn)
		conn.addMessageBus(d,bus)
		bus
  })
	def getHostname = hostname
	def getUsername = username
	def getPassword = password
	def getDomainName = domainName
}

class XmppConnProvider(configName:String,hostname:String,username:String,password:String,domainName:String) {
	private var conns = List.empty[MeTL2011XmppMultiConn]	
	private val maxCount = 20	
	def getConn:MeTL2011XmppMultiConn = {
		//println("XMPPConnProvider:getConn")
		conns.find(c => c.getCount < maxCount).getOrElse({
			val newConn = new MeTL2011XmppMultiConn(username,password,"metlxConnector_%s_%s".format(username, new Date().getTime.toString),hostname,domainName,configName,this)
			conns = newConn :: conns
			//println("XMPPConnProvider:getConn.createConn(%s)".format(newConn))
			newConn
		})
	}
	def releaseConn(c:MeTL2011XmppMultiConn) = {
		//println("XMPPConnProvider:releaseConn")
		if (c.getCount < 1){
			conns = conns.filterNot(conn => conn == c)
			//println("XMPPConnProvider:releaseConn.disconnectingConn(%s)".format(c))
    	c.disconnectFromXmpp
		}
	}
}

class MeTL2011XmppMultiConn(u:String,p:String,r:String,h:String,d:String,configName:String,creator:XmppConnProvider) extends XmppConnection[MeTLStanza](u,p,r,h,d,None){
	protected lazy val serializer = new MeTL2011XmlSerializer(configName,true)
	private lazy val config = ServerConfiguration.configForName(configName)
	
//  override lazy val debug = true

	private val subscribedBusses = new HashMap[String,HashMap[MessageBusDefinition,MessageBus]]
	
	private var subscriptionCount = 0

	def getCount = subscriptionCount

	override def onConnLost = {
		subscribedBusses.values.foreach(sbl => sbl.keys.foreach(mbd => mbd.onConnectionLost()))
	}
	override def onConnRegained = {
		subscribedBusses.values.foreach(sbl => sbl.keys.foreach(mbd => mbd.onConnectionRegained()))
	}	

	def addMessageBus(d:MessageBusDefinition,m:MessageBus) = {
		//println("XMPPMultiConn(%s):addMessageBus(%s)".format(this,d))
		val oldLocMap = subscribedBusses.get(d.location).getOrElse(HashMap.empty[MessageBusDefinition,MessageBus])
		oldLocMap.put(d,m) match {
			case Some(_) => {}
			case None => subscriptionCount += 1
		}
		subscribedBusses.put(d.location,oldLocMap)
	}
	def removeMessageBus(d:MessageBusDefinition) = {
		//println("XMPPMultiConn(%s):removeMessageBus(%s)".format(this,d))
		subscriptionCount -= 1
		subscribedBusses(d.location).remove(d)
		creator.releaseConn(this)
	}
	override def onMessageRecieved(room:String, messageType:String, message:MeTLStanza) = {
		//println("XMPPMultiConn(%s):onMessageReceived(%s,%s)".format(this,room,messageType))
		val targets = subscribedBusses(room).values
		//println("XMPPMultiConn(%s):onMessageReceived.sendTo(%s)".format(this,targets))
		targets.foreach(mb => mb.recieveStanzaFromRoom(message))
	}
	override def onUntypedMessageRecieved(room:String,message:String) = {
    val parts = message.split(" ")
		//println("XMPPMultiConn(%s):onUntypedMessageReceived(%s,%s,%s)".format(this,room,message))
		val targets = subscribedBusses(room).values
		//println("XMPPMultiConn(%s):onUntypedMessageReceived.sendTo(%s)".format(this,targets))
		targets.foreach(mb => mb.recieveStanzaFromRoom(MeTLCommand(config,"unknown",new java.util.Date().getTime,parts.head,parts.tail.toList)))
	}
	override lazy val ignoredTypes = List("metlMetaData")
  override lazy val subscribedTypes = List("ink","textbox","image","dirtyInk","dirtyText","dirtyImage","screenshotSubmission","submission","quiz","quizResponse","command","moveDelta","teacherstatus").map(item => {
    val ser = (i:MeTLStanza) => {
      val xml = serializer.fromMeTLStanza(i)
      val messages = xml
      val head = messages.headOption
      head.map{
        case g:Group => g.nodes.headOption.getOrElse(NodeSeq.Empty)
        case e:Elem => e.child.headOption.getOrElse(NodeSeq.Empty)
      }.getOrElse(NodeSeq.Empty)
    }
    val deser = (s:NodeSeq) => {
      serializer.toMeTLStanza(s)
    }
    XmppDataType[MeTLStanza](item,ser,deser)
  })
}

class MeTL2011XmppConn(u:String,p:String,r:String,h:String,d:String,configName:String,bus:MessageBus) extends XmppConnection[MeTLStanza](u,p,r,h,d,None,bus.notifyConnectionLost _,bus.notifyConnectionResumed _){
  protected lazy val serializer = new MeTL2011XmlSerializer(configName,true)
  private lazy val config = ServerConfiguration.configForName(configName)

  //    override lazy val debug = true

  override def onMessageRecieved(room:String, messageType:String, message:MeTLStanza) = {
    bus.recieveStanzaFromRoom(message)
  }
  override def onUntypedMessageRecieved(room:String,message:String) = {
    val parts = message.split(" ")
    bus.recieveStanzaFromRoom(MeTLCommand(config,"unknown",new java.util.Date().getTime,parts.head,parts.tail.toList))
  }
  override lazy val ignoredTypes = List("metlMetaData")
  override lazy val subscribedTypes = List("ink","textbox","image","dirtyInk","dirtyText","dirtyImage","screenshotSubmission","submission","quiz","quizResponse","command","moveDelta","teacherstatus").map(item => {
    val ser = (i:MeTLStanza) => {
      val xml = serializer.fromMeTLStanza(i)
      val messages = xml
      val head = messages.headOption
      head.map{
        case g:Group => g.nodes.headOption.getOrElse(NodeSeq.Empty)
        case e:Elem => e.child.headOption.getOrElse(NodeSeq.Empty)
      }.getOrElse(NodeSeq.Empty)
    }
    val deser = (s:NodeSeq) => {
      serializer.toMeTLStanza(s)
    }
    XmppDataType[MeTLStanza](item,ser,deser)
  })
}

class XmppSharedConnMessageBus(configName:String,hostname:String,username:String,password:String,domain:String,d:MessageBusDefinition,creator:MessageBusProvider) extends MessageBus(d,creator){
  val jid = d.location
	private var xmpp:Option[MeTL2011XmppMultiConn] = None
	def addConn(conn:MeTL2011XmppMultiConn) = {
		//println("XMPPSharedConnMessageBus(%s):addConn(%s)".format(d,conn))
		xmpp = Some(conn)
		xmpp.map(x => x.joinRoom(jid,this.hashCode.toString))
	}
  override def sendStanzaToRoom(stanza:MeTLStanza):Boolean = Stopwatch.time("XmppSharedConnMessageBus.sendStanzaToRoom", () => {
		//println("XMPPSharedConnMessageBus(%s):sendStanzaToRoom(%s)".format(d,xmpp))
		stanza match {
			case i:MeTLInk =>{
				xmpp.map(x => x.sendMessage(jid,"ink",i))
				true}
			case t:MeTLText =>{
				xmpp.map(x => x.sendMessage(jid,"textbox",t))
				true}
			case i:MeTLImage =>{
				xmpp.map(x => x.sendMessage(jid,"image",i))
				true}
			case di:MeTLDirtyInk => {
				xmpp.map(x => x.sendMessage(jid,"dirtyInk",di))
				true}
			case dt:MeTLDirtyText =>{
				xmpp.map(x => x.sendMessage(jid,"dirtyText",dt))
				true}
			case di:MeTLDirtyImage =>{
				xmpp.map(x => x.sendMessage(jid,"dirtyImage",di))
				true}
			case q:MeTLQuiz =>{
				xmpp.map(x => x.sendMessage(jid,"quiz",q))
				true}
			case qr:MeTLQuizResponse =>{
				xmpp.map(x => x.sendMessage(jid,"quizResponse",qr))
				true}
			case s:MeTLSubmission =>{
				xmpp.map(x => x.sendMessage(jid,"submission",s))
				true}
			case c:MeTLCommand =>{
				xmpp.map(x => x.sendSimpleMessage(jid,(c.command :: c.commandParameters).mkString(" ")))
				true}
			case d:MeTLMoveDelta =>{
				xmpp.map(x => x.sendMessage(jid,"moveDelta",d))
				true}
			case _ => {
				false
			}
		}
  })
  override def release = {
		//println("XMPPSharedConnMessageBus(%s):release".format(d))
		xmpp.map(x => x.leaveRoom(jid,this.hashCode.toString))
		xmpp.map(x => x.removeMessageBus(d))
    super.release
  }
}
class XmppMessageBus(configName:String,hostname:String,username:String,password:String,domain:String,d:MessageBusDefinition,creator:MessageBusProvider) extends MessageBus(d,creator){
  val jid = d.location
  lazy val xmpp = new MeTL2011XmppConn(username,password,"metlxConnector_%s_%s".format(username, new Date().getTime.toString),hostname,domain,configName,this)
  xmpp.joinRoom(jid,this.hashCode.toString)
  override def sendStanzaToRoom(stanza:MeTLStanza):Boolean = stanza match {
    case i:MeTLInk =>{
      xmpp.sendMessage(jid,"ink",i)
      true}
    case t:MeTLText =>{
      xmpp.sendMessage(jid,"textbox",t)
      true}
    case i:MeTLImage =>{
      xmpp.sendMessage(jid,"image",i)
      true}
    case di:MeTLDirtyInk => {
      xmpp.sendMessage(jid,"dirtyInk",di)
      true}
    case dt:MeTLDirtyText =>{
      xmpp.sendMessage(jid,"dirtyText",dt)
      true}
    case di:MeTLDirtyImage =>{
      xmpp.sendMessage(jid,"dirtyImage",di)
      true}
    case q:MeTLQuiz =>{
      xmpp.sendMessage(jid,"quiz",q)
      true}
    case qr:MeTLQuizResponse =>{
      xmpp.sendMessage(jid,"quizResponse",qr)
      true}
    case s:MeTLSubmission =>{
      xmpp.sendMessage(jid,"submission",s)
      true}
    case c:MeTLCommand =>{
      xmpp.sendSimpleMessage(jid,(c.command :: c.commandParameters).mkString(" "))
      true}
    case d:MeTLMoveDelta =>{
      xmpp.sendMessage(jid,"moveDelta",d)
      true}
    case _ => {
      false
    }
  }
  override def release = {
    xmpp.disconnectFromXmpp
    super.release
  }
}
