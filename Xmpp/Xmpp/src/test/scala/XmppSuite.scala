package com.metl

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.util.Helpers._

import org.scalatest._
import org.scalatest.mock.MockitoSugar
import org.scalatest.OptionValues._
import org.scalatest.prop.TableDrivenPropertyChecks._

import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any, anyInt}

import org.jivesoftware.smack._
import org.jivesoftware.smack.packet.Presence
import java.util.Date
import com.metl.utils._ 
import com.metl.xmpp._

class TestXmppConnection(user: String, pass: String, res: String, host: String, xmppConn: Box[XMPPConnection]) extends XmppConnection[String](user, pass, res, host, xmppConn) {

	override def onMessageRecieved(room:String, messageType:String, message:String) = {
	}

	override def onUntypedMessageRecieved(room:String,message:String) = {
	}
}

class XmppSuite extends FunSuite with MockitoSugar {

    val resource = "testConnector_%s_%s".format("username", new Date().getTime.toString)
    val hostname = "test.metl.com"
    val user = "username"
    val pass = "password"

    test("create an xmpp connection") { 

        val xmppConnection = mock[XMPPConnection]
        val xmppConn = new TestXmppConnection(user, pass, resource, hostname, tryo(xmppConnection))
        verify(xmppConnection).connect
        verify(xmppConnection).login(user, pass, resource)
    }

    test("create and disconnect from xmpp connection") {

        val xmppConnection = mock[XMPPConnection]
        val xmppConn = new TestXmppConnection(user, pass, resource, hostname, tryo(xmppConnection))

        verify(xmppConnection).connect
        verify(xmppConnection).login(user, pass, resource) 

        xmppConn.disconnectFromXmpp

        verify(xmppConnection).disconnect(new Presence(Presence.Type.unavailable))
    }

}

