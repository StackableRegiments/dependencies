package com.metl

import net.liftweb.util._
import net.liftweb.common._

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.mock.MockitoSugar
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.concurrent.AsyncAssertions
import org.scalatest.OptionValues._

import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any, anyInt}

import java.util.concurrent.TimeUnit
import org.apache.http.{HttpResponse, HttpStatus, ProtocolVersion}
import org.apache.http.entity.StringEntity
import org.apache.http.conn.{ClientConnectionManager, ManagedClientConnection, ClientConnectionRequest}
import org.apache.http.conn.routing.HttpRoute
import org.apache.http.message.{BasicStatusLine, BasicHeader, BasicHttpResponse}

import com.metl.utils._ 

class HttpClientSuite extends FunSuite with AsyncAssertions with ShouldMatchers with MockitoSugar with BeforeAndAfter {

    var connectionManager : ClientConnectionManager = _ 

    before {
        connectionManager = mock[ClientConnectionManager]   
    }

    test("create clean http client") {

        var client = new CleanHttpClient(connectionManager)
    }

    test("shutdown clean http client") {
        
        var client = new CleanHttpClient(connectionManager)

        client.getConnectionManager.shutdown

        verify(connectionManager).shutdown
    }

    test("handles empty string as uri gracefully") {
        
        var client = new CleanHttpClient(connectionManager)

        intercept[IllegalArgumentException] {

          var result = client.get("")
        }
    }

    test("handles junk string as uri gracefully") {
        
        var client = new CleanHttpClient(connectionManager)

        intercept[IllegalArgumentException] {

          var result = client.get("laksdjflkjwefjvjke")
        }
    }

    test("handle socket timeout gracefully") {
        
        var client = new CleanHttpClient(connectionManager)
        var conn = mock[ManagedClientConnection]

        var connRequest = mock[ClientConnectionRequest]

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connectionManager.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(false)
         
        client.get("http://test.metl.com/data.xml")
    }

    test("response available and has status code of ok") {
        
        var client = new CleanHttpClient(connectionManager)
        var conn = mock[ManagedClientConnection]

        var connRequest = mock[ClientConnectionRequest]

        var response = prepareHttpResponse("Whatever", HttpStatus.SC_OK)

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connectionManager.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(true)
        when(conn.receiveResponseHeader).thenReturn(response)
         
        var result = client.get("http://test.metl.com/data.xml")
        assert(result === "Whatever")
    }

    private def prepareHttpResponse(expectedBody: String, expectedStatusCode: Int): HttpResponse = {
    
        var response = new BasicHttpResponse(new BasicStatusLine(new ProtocolVersion("HTTP", 1, 1), expectedStatusCode, ""))
        response.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))
        response.setEntity(new StringEntity(expectedBody))
        response.setStatusCode(expectedStatusCode)
        response
    }
}
