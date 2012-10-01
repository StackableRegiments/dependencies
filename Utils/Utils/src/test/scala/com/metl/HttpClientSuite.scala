package com.metl

import net.liftweb.util._
import net.liftweb.common._

import org.scalatest.fixture
import org.scalatest.fixture.ConfigMapFixture
import org.scalatest.mock.MockitoSugar
import org.scalatest.OptionValues._

import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any, anyInt}

import java.util.concurrent.TimeUnit
import org.apache.http.{HttpResponse, HttpStatus, HttpVersion, ProtocolVersion, HttpRequest, HttpEntityEnclosingRequest}
import org.apache.http.entity.StringEntity
import org.apache.http.conn.{ClientConnectionManager, ManagedClientConnection, ClientConnectionRequest}
import org.apache.http.conn.routing.HttpRoute
import org.apache.http.message.{BasicStatusLine, BasicHeader, BasicHttpResponse}

import com.metl.utils._ 

class HttpClientSuite extends fixture.FunSuite with ConfigMapFixture with MockitoSugar {

    case class F(connMgr: ClientConnectionManager, client: CleanHttpClient, conn: ManagedClientConnection, connRequest: ClientConnectionRequest)

    def withClient(test: F => Any) {

        val connMgr = mock[ClientConnectionManager]
        val client = new CleanHttpClient(connMgr)
        val conn = mock[ManagedClientConnection]
        val connRequest = mock[ClientConnectionRequest]

        val fixture = F(connMgr, client, conn, connRequest)
        test(fixture)
    }

    def withConnection(test: F => Any) {

        val connMgr = mock[ClientConnectionManager]
        val client = new CleanHttpClient(connMgr)
        val conn = mock[ManagedClientConnection]
        val connRequest = mock[ClientConnectionRequest]

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connMgr.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)

        val fixture = F(connMgr, client, conn, connRequest)

        test(fixture)
    }

    test("shutdown clean http client") { () => 
        withClient { f =>
        
            f.client.getConnectionManager.shutdown

            verify(f.connMgr).shutdown
        }
    }

    test("handles empty string as uri gracefully") { () =>
        withClient { f =>
        
            intercept[IllegalArgumentException] {

                val requestedUri = ""
                val expectedResult = ""

                val result1a = f.client.get(requestedUri)
                assert(result1a === expectedResult)

                val result2a = f.client.getAsBytes(requestedUri)
                assert(result2a === expectedResult.toCharArray.map(_.toByte))

                val result3a = f.client.getAsString(requestedUri)
                assert(result3a === expectedResult)
            }
        }
    }

    test("handles empty string as uri gracefully with additional header") { () =>
        withClient { f =>
        
            intercept[IllegalArgumentException] {

                val requestedUri = ""
                val expectedResult = ""
                val additionalHeader = List(("Accept", "text/plain"))

                val result1b = f.client.get(requestedUri, additionalHeader)
                assert(result1b === expectedResult)

                val result2b = f.client.getAsBytes(requestedUri, additionalHeader)
                assert(result2b === expectedResult.toCharArray.map(_.toByte))

                val result3b = f.client.getAsString(requestedUri, additionalHeader)
                assert(result3b === expectedResult)
            }
        }
    }

    test("handles junk string as uri gracefully") { () =>
        withClient { f =>
        
            intercept[IllegalArgumentException] {

                val requestedUri = "garbage"
                val expectedResult = ""

                val result1a = f.client.get(requestedUri)
                assert(result1a === expectedResult)

                val result2a = f.client.getAsBytes(requestedUri)
                assert(result2a === expectedResult.toCharArray.map(_.toByte))

                val result3a = f.client.getAsString(requestedUri)
                assert(result3a === expectedResult)
            }
        }
    }

    test("handles junk string as uri gracefully with additional header") { () =>
        withClient { f =>
        
            intercept[IllegalArgumentException] {

                val requestedUri = "garbage"
                val expectedResult = ""
                val additionalHeader = List(("Accept", "text/plain"))

                val result1b = f.client.get(requestedUri, additionalHeader)
                assert(result1b === expectedResult)

                val result2b = f.client.getAsBytes(requestedUri, additionalHeader)
                assert(result2b === expectedResult.toCharArray.map(_.toByte))

                val result3b = f.client.getAsString(requestedUri, additionalHeader)
                assert(result3b === expectedResult)
            }
        }
    }

    test("handle socket timeout gracefully") { () =>
        withConnection { f =>
        
            when(f.conn.isResponseAvailable(anyInt)).thenReturn(false)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = ""

            val result1a = f.client.get(requestedUri)
            assert(result1a === expectedResult)

            val result2a = f.client.getAsBytes(requestedUri)
            assert(result2a === expectedResult.toCharArray.map(_.toByte))

            val result3a = f.client.getAsString(requestedUri)
            assert(result3a === expectedResult)
        }
    }

    test("handle socket timeout gracefully with additional header") { () =>
        withConnection { f =>
        
            when(f.conn.isResponseAvailable(anyInt)).thenReturn(false)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = ""
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.get(requestedUri, additionalHeader)
            assert(result1b === expectedResult)

            val result2b = f.client.getAsBytes(requestedUri, additionalHeader)
            assert(result2b === expectedResult.toCharArray.map(_.toByte))

            val result3b = f.client.getAsString(requestedUri, additionalHeader)
            assert(result3b === expectedResult)
        }
    }

    test("response available and has status code of ok") { () =>
        withConnection { f => 

            var response = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            response.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(response)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"

            val result1a = f.client.get(requestedUri)
            assert(result1a === expectedResult)

            val result2a = f.client.getAsBytes(requestedUri)
            assert(result2a === expectedResult.toCharArray.map(_.toByte))

            val result3a = f.client.getAsString(requestedUri)
            assert(result3a === expectedResult)
        }
    }

    test("response available and has status code of ok with additional header") { () =>
        withConnection { f => 

            var response = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            response.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(response)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.get(requestedUri, additionalHeader)
            assert(result1b === expectedResult)

            val result2b = f.client.getAsBytes(requestedUri, additionalHeader)
            assert(result2b === expectedResult.toCharArray.map(_.toByte))

            val result3b = f.client.getAsString(requestedUri, additionalHeader)
            assert(result3b === expectedResult)
        }
    }

    test("response available but has no cookies") { () =>
        withConnection { f =>
        
            var response = prepareHttpResponse("Whatever", HttpStatus.SC_OK)

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(response)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"

            val result1a = f.client.get(requestedUri)
            assert(result1a === expectedResult)

            val result2a = f.client.getAsBytes(requestedUri)
            assert(result2a === expectedResult.toCharArray.map(_.toByte))

            val result3a = f.client.getAsString(requestedUri)
            assert(result3a === expectedResult)
        }
    }

    test("response available but has no cookies with additional header") { () =>
        withConnection { f =>
        
            var response = prepareHttpResponse("Whatever", HttpStatus.SC_OK)

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(response)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.get(requestedUri, additionalHeader)
            assert(result1b === expectedResult)

            val result2b = f.client.getAsBytes(requestedUri, additionalHeader)
            assert(result2b === expectedResult.toCharArray.map(_.toByte))

            val result3b = f.client.getAsString(requestedUri, additionalHeader)
            assert(result3b === expectedResult)
        }
    }

    test("handle unimplemented status code") { () =>
        withConnection { f => 

            var response = prepareHttpResponse("Ignored", HttpStatus.SC_NO_CONTENT)

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(response)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = ""

            val result1a = f.client.get(requestedUri)
            assert(result1a === expectedResult)

            val result2a = f.client.getAsBytes(requestedUri)
            assert(result2a === expectedResult.toCharArray.map(_.toByte))

            val result3a = f.client.getAsString(requestedUri)
            assert(result3a === expectedResult)
        }
    }

    test("handle unimplemented status code with additional header") { () =>
        withConnection { f => 

            var response = prepareHttpResponse("Ignored", HttpStatus.SC_NO_CONTENT)

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(response)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = ""
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.get(requestedUri, additionalHeader)
            assert(result1b === expectedResult)

            val result2b = f.client.getAsBytes(requestedUri, additionalHeader)
            assert(result2b === expectedResult.toCharArray.map(_.toByte))

            val result3b = f.client.getAsString(requestedUri, additionalHeader)
            assert(result3b === expectedResult)
        }
    }

    test("handle redirect to invalid uri") { () =>
        withConnection { f =>

            var redirectResponse = prepareHttpResponse("Redirection", HttpStatus.SC_MOVED_PERMANENTLY)
            redirectResponse.addHeader(new BasicHeader("Location", "lkjlasdoifljsf"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(redirectResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = ""

            val result1a = f.client.get(requestedUri)
            assert(result1a === expectedResult)

            val result2a = f.client.getAsBytes(requestedUri)
            assert(result2a === expectedResult.toCharArray.map(_.toByte))

            val result3a = f.client.getAsString(requestedUri)
            assert(result3a === expectedResult)
        }
    }

    test("handle redirect to invalid uri with additional header") { () =>
        withConnection { f =>

            var redirectResponse = prepareHttpResponse("Redirection", HttpStatus.SC_MOVED_PERMANENTLY)
            redirectResponse.addHeader(new BasicHeader("Location", "lkjlasdoifljsf"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(redirectResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = ""
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.get(requestedUri, additionalHeader)
            assert(result1b === expectedResult)

            val result2b = f.client.getAsBytes(requestedUri, additionalHeader)
            assert(result2b === expectedResult.toCharArray.map(_.toByte))

            val result3b = f.client.getAsString(requestedUri, additionalHeader)
            assert(result3b === expectedResult)
        }
    }

    test("response available with redirect status code") { () =>
        withConnection { f => 

            var redirectResponse = prepareHttpResponse("Redirection", HttpStatus.SC_MULTIPLE_CHOICES)
            redirectResponse.addHeader(new BasicHeader("Location", "http://test2.metl.com/redirect.xml"))

            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(redirectResponse).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"

            val result1a = f.client.get(requestedUri)
            assert(result1a === expectedResult)

            val result2a = f.client.getAsBytes(requestedUri)
            assert(result2a === expectedResult.toCharArray.map(_.toByte))

            val result3a = f.client.getAsString(requestedUri)
            assert(result3a === expectedResult)
        }
    }

    test("response available with redirect status code with additional header") { () =>
        withConnection { f => 

            var redirectResponse = prepareHttpResponse("Redirection", HttpStatus.SC_MULTIPLE_CHOICES)
            redirectResponse.addHeader(new BasicHeader("Location", "http://test2.metl.com/redirect.xml"))

            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(redirectResponse).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.get(requestedUri, additionalHeader)
            assert(result1b === expectedResult)

            val result2b = f.client.getAsBytes(requestedUri, additionalHeader)
            assert(result2b === expectedResult.toCharArray.map(_.toByte))

            val result3b = f.client.getAsString(requestedUri, additionalHeader)
            assert(result3b === expectedResult)
        }
    }

    test("response available after socket timeout") { () =>
        withConnection { f =>
        
            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(false).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"

            val result1a = f.client.get(requestedUri)
            assert(result1a === expectedResult)

            val result2a = f.client.getAsBytes(requestedUri)
            assert(result2a === expectedResult.toCharArray.map(_.toByte))

            val result3a = f.client.getAsString(requestedUri)
            assert(result3a === expectedResult)
        }
    }

    test("response available after socket timeout with additional header") { () =>
        withConnection { f =>
        
            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(false).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.get(requestedUri, additionalHeader)
            assert(result1b === expectedResult)

            val result2b = f.client.getAsBytes(requestedUri, additionalHeader)
            assert(result2b === expectedResult.toCharArray.map(_.toByte))

            val result3b = f.client.getAsString(requestedUri, additionalHeader)
            assert(result3b === expectedResult)
        }
    }

    test("response available after unimplemented statuscode exception") { () =>
        withConnection { f => 

            var unimplementedResponse = prepareHttpResponse("Unimplemented", HttpStatus.SC_USE_PROXY)

            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(unimplementedResponse).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"

            val result1a = f.client.get(requestedUri)
            assert(result1a === expectedResult)

            val result2a = f.client.getAsBytes(requestedUri)
            assert(result2a === expectedResult.toCharArray.map(_.toByte))

            val result3a = f.client.getAsString(requestedUri)
            assert(result3a === expectedResult)
        }
    }

    test("response available after unimplemented statuscode exception with additional header") { () =>
        withConnection { f => 

            var unimplementedResponse = prepareHttpResponse("Unimplemented", HttpStatus.SC_USE_PROXY)

            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(unimplementedResponse).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.get(requestedUri, additionalHeader)
            assert(result1b === expectedResult)

            val result2b = f.client.getAsBytes(requestedUri, additionalHeader)
            assert(result2b === expectedResult.toCharArray.map(_.toByte))

            val result3b = f.client.getAsString(requestedUri, additionalHeader)
            assert(result3b === expectedResult)
        }
    }

    test("post bytes using the connection") { () =>
        withConnection { f =>
            
            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"

            val result1a = f.client.postBytes(requestedUri, expectedResult.toCharArray.map(_.toByte))
            assert(result1a === expectedResult.toCharArray.map(_.toByte))

            verify(f.conn).sendRequestHeader(any(classOf[HttpRequest]))
            verify(f.conn).sendRequestEntity(any(classOf[HttpEntityEnclosingRequest]))
            verify(f.conn).flush
        }
    }

    test("post bytes using the connection with additional header") { () =>
        withConnection { f =>
            
            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.postBytes(requestedUri, expectedResult.toCharArray.map(_.toByte), additionalHeader)
            assert(result1b === expectedResult.toCharArray.map(_.toByte))

            verify(f.conn).sendRequestHeader(any(classOf[HttpRequest]))
            verify(f.conn).sendRequestEntity(any(classOf[HttpEntityEnclosingRequest]))
            verify(f.conn).flush
        }
    }

    test("post form using the connection") { () =>
        withConnection { f =>
            
            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"

            val result1a = f.client.postForm(requestedUri, List(("FirstName", "Bob"), ("LastName", "Barry"), ("Age", "35")))
            assert(result1a === expectedResult.toCharArray.map(_.toByte))

            verify(f.conn).sendRequestHeader(any(classOf[HttpRequest]))
            verify(f.conn).sendRequestEntity(any(classOf[HttpEntityEnclosingRequest]))
            verify(f.conn).flush
        }
    }

    test("post form using the connection with additional header") { () =>
        withConnection { f =>
            
            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.postForm(requestedUri, List(("FirstName", "Bob"), ("LastName", "Barry"), ("Age", "35")), additionalHeader)
            assert(result1b === expectedResult.toCharArray.map(_.toByte))

            verify(f.conn).sendRequestHeader(any(classOf[HttpRequest]))
            verify(f.conn).sendRequestEntity(any(classOf[HttpEntityEnclosingRequest]))
            verify(f.conn).flush
        }
    }

    test("post unencoded form using the connection") { () =>
        withConnection { f =>
            
            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"

            val result1a = f.client.postUnencodedForm(requestedUri, List(("FirstName", "Bob"), ("LastName", "Barry"), ("Age", "35")))
            assert(result1a === expectedResult.toCharArray.map(_.toByte))

            verify(f.conn).sendRequestHeader(any(classOf[HttpRequest]))
            verify(f.conn).sendRequestEntity(any(classOf[HttpEntityEnclosingRequest]))
            verify(f.conn).flush
        }
    }

    test("post unencoded form using the connection with additional header") { () =>
        withConnection { f =>
            
            var contentResponse = prepareHttpResponse("Whatever", HttpStatus.SC_OK)
            contentResponse.addHeader(new BasicHeader("Set-Cookie", "UserID=testing"))

            when(f.conn.isResponseAvailable(anyInt)).thenReturn(true)
            when(f.conn.receiveResponseHeader).thenReturn(contentResponse)
         
            val requestedUri = "http://test.metl.com/data.xml"
            val expectedResult = "Whatever"
            val additionalHeader = List(("Accept", "text/plain"))

            val result1b = f.client.postUnencodedForm(requestedUri, List(("FirstName", "Bob"), ("LastName", "Barry"), ("Age", "35")), additionalHeader)
            assert(result1b === expectedResult.toCharArray.map(_.toByte))

            verify(f.conn).sendRequestHeader(any(classOf[HttpRequest]))
            verify(f.conn).sendRequestEntity(any(classOf[HttpEntityEnclosingRequest]))
            verify(f.conn).flush
        }
    }

    private def prepareHttpResponse(expectedBody: String, expectedStatusCode: Int): HttpResponse = {
    
        var response = new BasicHttpResponse(HttpVersion.HTTP_1_1, expectedStatusCode, "OK")
        response.setEntity(new StringEntity(expectedBody))
        response.setStatusCode(expectedStatusCode)
        response
    }
}
