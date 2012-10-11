package com.metl

import scala.xml.NodeSeq

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

	override lazy val subscribedTypes = List("string").map(item => {
		val ser = (i:String) => {
            <string>i</string>
		}
		val deser = (s:NodeSeq) => {
		    s.text	
		}
		XmppDataType[String](item,ser,deser)
	})
}

class XmppSuite extends FunSuite with MockitoSugar {

    val resource = "testConnector_%s_%s".format("username", new Date().getTime.toString)
    val hostname = "test.metl.com"
    val user = "username"
    val pass = "password"

    case class XmppConn(conn: XMPPConnection, controller: TestXmppConnection)

    def createXmppConnection: XmppConn = {

        val xmppConnection = mock[XMPPConnection]
        val xmppConn = new TestXmppConnection(user, pass, resource, hostname, tryo(xmppConnection))
        XmppConn(xmppConnection, xmppConn)
    }

    test("create an xmpp connection") { 

        val xmpp = createXmppConnection
        verify(xmpp.conn).connect
        verify(xmpp.conn).login(user, pass, resource)
    }

    test("create and disconnect from xmpp connection") {

        val xmpp = createXmppConnection

        verify(xmpp.conn).connect
        verify(xmpp.conn).login(user, pass, resource) 

        xmpp.controller.disconnectFromXmpp

        verify(xmpp.conn).disconnect(new Presence(Presence.Type.unavailable))
    }

    test("send a message when not connected to a room") (pending)
    test("join room") (pending)
    test("leave Room") (pending)
    test("send message") (pending)
    test("send simple message") (pending)
    test("xmpp data type : generate packet extension") (pending)
    test("xmpp data type : comprehend response") (pending)
    test("metl extension provider : parse extension") (pending)
}
