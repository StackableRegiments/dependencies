package com.metl.model

class XmppProvider(configName:String,hostname:String,username:String,password:String) extends MessageBusProvider(configName){
	override def getMessageBus(jid:String) = new XmppMessageBus(username,password,hostname,jid,configName)
}

class XmppMessageBus(username:String,password:String,host:String,jid:String,configName:String) extends MessageBus(jid,configName){
	private val xmpp = new XMPPSyncActor
	xmpp ! InitializeXMPP(host,username,password,jid)
	override def sendStanzaToRoom(stanza:MeTLStanza) = xmpp ! MeTLStanzaSyncRequest(stanza)
}
