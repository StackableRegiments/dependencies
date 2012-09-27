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

class HttpClientSuite extends FunSuite with MockitoSugar with BeforeAndAfter {

    var connectionManager : ClientConnectionManager = _ 

    before {
        connectionManager = mock[ClientConnectionManager]   
        // test additional headers calls with tuple ("Accept", "text/plain")
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
          assert(result === "")
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
         
        val result = client.get("http://test.metl.com/data.xml")
        assert(result === "")
    }

    test("response available and has status code of ok") {
        
        var client = new CleanHttpClient(connectionManager)
        var conn = mock[ManagedClientConnection]

        var connRequest = mock[ClientConnectionRequest]

        var response = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
        response.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connectionManager.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(true)
        when(conn.receiveResponseHeader).thenReturn(response)
         
        val result = client.get("http://test.metl.com/data.xml")
        assert(result === "Whatever")
    }

    test("response available but has no cookies") {
        
        var client = new CleanHttpClient(connectionManager)
        var conn = mock[ManagedClientConnection]

        var connRequest = mock[ClientConnectionRequest]

        var response = prepareHttpResponse("Whatever", HttpStatus.SC_OK)

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connectionManager.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(true)
        when(conn.receiveResponseHeader).thenReturn(response)
         
        val result = client.get("http://test.metl.com/data.xml")
        assert(result === "Whatever")
    }

    test("handle unimplemented status code") {
        
        var client = new CleanHttpClient(connectionManager)
        var conn = mock[ManagedClientConnection]

        var connRequest = mock[ClientConnectionRequest]

        var response = prepareHttpResponse("Whatever", HttpStatus.SC_NO_CONTENT)

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connectionManager.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(true)
        when(conn.receiveResponseHeader).thenReturn(response)
         
        val result = client.get("http://test.metl.com/data.xml")
        assert(result === "")
    }

    test("handle redirect to invalid uri") {
        
        var client = new CleanHttpClient(connectionManager)
        var conn = mock[ManagedClientConnection]

        var connRequest = mock[ClientConnectionRequest]

        var redirectResponse = prepareHttpResponse("Redirection", HttpStatus.SC_MOVED_PERMANENTLY)
        redirectResponse.addHeader(new BasicHeader("Location", "lkjlasdoifljsf"))

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connectionManager.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(true)
        when(conn.receiveResponseHeader).thenReturn(redirectResponse)
         
        val result = client.get("http://test.metl.com/data.xml")
        assert(result === "")
    }

    test("response available with redirect status code") {
        
        var client = new CleanHttpClient(connectionManager)
        var conn = mock[ManagedClientConnection]

        var connRequest = mock[ClientConnectionRequest]

        var redirectResponse = prepareHttpResponse("Redirection", HttpStatus.SC_MULTIPLE_CHOICES)
        redirectResponse.addHeader(new BasicHeader("Location", "http://test2.metl.com/redirect.xml"))

        var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
        contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connectionManager.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(true)
        when(conn.receiveResponseHeader).thenReturn(redirectResponse).thenReturn(contentResponse)
         
        val result = client.get("http://test.metl.com/data.xml")
        assert(result === "Whatever")
    }

    test("response available after socket timeout") {
        
        var client = new CleanHttpClient(connectionManager)
        var conn = mock[ManagedClientConnection]

        var connRequest = mock[ClientConnectionRequest]

        var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
        contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connectionManager.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(false).thenReturn(true)
        when(conn.receiveResponseHeader).thenReturn(contentResponse)
         
        val result = client.get("http://test.metl.com/data.xml")
        assert(result === "Whatever")
    }

    test("response available after unimplemented statuscode exception") {
        
        var client = new CleanHttpClient(connectionManager)
        var conn = mock[ManagedClientConnection]

        var connRequest = mock[ClientConnectionRequest]

        var unimplementedResponse = prepareHttpResponse("Unimplemented", HttpStatus.SC_USE_PROXY)

        var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
        contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connectionManager.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(true)
        when(conn.receiveResponseHeader).thenReturn(unimplementedResponse).thenReturn(contentResponse)
         
        val result = client.get("http://test.metl.com/data.xml")
        assert(result === "Whatever")
    }

    private def prepareHttpResponse(expectedBody: String, expectedStatusCode: Int): HttpResponse = {
    
        var response = new BasicHttpResponse(new BasicStatusLine(new ProtocolVersion("HTTP", 1, 1), expectedStatusCode, ""))
        response.setEntity(new StringEntity(expectedBody))
        response.setStatusCode(expectedStatusCode)
        response
    }
}
