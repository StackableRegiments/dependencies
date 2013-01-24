package com.metl.metl2011

import com.metl.data._
import com.metl.utils._
import com.metl.xmpp._

import java.util.Random
import net.liftweb.util.Helpers._
import scala.xml._
import java.util.Date

class XmppProvider(configName:String,hostname:String,username:String,password:String,domainName:String) extends OneBusPerRoomMessageBusProvider{
  override def createNewMessageBus(d:MessageBusDefinition) = Stopwatch.time("XmppProvider.createNewMessageBus", () => {
    new XmppMessageBus(configName,hostname,username + new java.util.Date().getTime.toString,password,domainName,d,this)
  })
}

class MeTL2011XmppConn(u:String,p:String,r:String,h:String,d:String,configName:String,bus:MessageBus) extends XmppConnection[MeTLStanza](u,p,r,h,d){
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

class XmppMessageBus(configName:String,hostname:String,username:String,password:String,domain:String,d:MessageBusDefinition,creator:MessageBusProvider) extends MessageBus(d,creator){
  val jid = d.location
  lazy val xmpp = new MeTL2011XmppConn(username,password,"metlxConnector_%s_%s".format(username, new Date().getTime.toString),hostname,domain,configName,this)
  xmpp.joinRoom(jid)
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
