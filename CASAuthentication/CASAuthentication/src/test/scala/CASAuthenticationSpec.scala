package com.metl.cas

import com.metl.utils._
import javax.servlet.http.HttpServletRequest

import net.liftweb.common.{Box, Full, Empty}
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.mockweb.MockWeb
import net.liftweb.mockweb.WebSpec
import net.liftweb.http.{LiftRules, LiftRulesMocker, S, SessionVar}

import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any, anyInt}

import java.util.concurrent.TimeUnit
import java.io.IOException
import org.apache.http.{HttpResponse, HttpStatus, HttpVersion, ProtocolVersion, HttpRequest, HttpEntityEnclosingRequest, HttpException}
import org.apache.http.entity.StringEntity
import org.apache.http.conn.{ClientConnectionManager, ManagedClientConnection, ClientConnectionRequest}
import org.apache.http.conn.routing.HttpRoute
import org.apache.http.message.{BasicStatusLine, BasicHeader, BasicHttpResponse}

import org.junit.runner.RunWith
import org.specs.runner.{JUnitSuiteRunner, JUnit}

@RunWith(classOf[JUnitSuiteRunner])
class CASAuthenticationSpec extends WebSpec with JUnit with MockitoSugar {

    import net.liftweb.mockweb.MockWeb._
    "create a CASAuthenticator" in {

        val auth = new CASAuthenticator("realm",() => false, (cs:CASStateData) => 
        {
            println("CASAuthenticator::loginHandler")
        })
        assert(auth != null)
    }

    "check whether already logged in returns false when not logged in" in {

        val auth = new CASAuthenticator("realm",() => false, (cs:CASStateData) => 
        {
            println("CASAuthenticator::loginHandler")
        })

        auth.checkWhetherAlreadyLoggedIn must_== false
    }

    val testSession = MockWeb.testS("http://test.metl.edu/test") {
      S.session
    }

    case class F(connMgr: ClientConnectionManager, client: CleanHttpClient, conn: ManagedClientConnection, connRequest: ClientConnectionRequest)
    def testClient = {

        val connMgr = mock[ClientConnectionManager]
        val client = new CleanHttpClient(connMgr)
        val conn = mock[ManagedClientConnection]
        val connRequest = mock[ClientConnectionRequest]

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connMgr.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(true)

        F(connMgr, client, conn, connRequest)
    }

    //val mockReq = new MockHttpServletRequest("http://test.metl.edu/test/login?ticket=foo", "/test")

    object TestVar extends SessionVar[CASStateData](CASStateDataForbidden)

    "user log in unsuccessful with invalid request" in {
        testReq("http://test.metl.edu/test/this?foo=hbar", "/test") {

            req => {

             val httpClient = testClient
             val auth = new CASAuthenticator("realm", Some(httpClient.client), () => { println("CASAuthenticator::alreadyLoggedIn"); false }, (cs:CASStateData) => 
             {
                // onSuccess 
                println("onSuccess called")
             })

             auth.checkReqForCASCookies(req) must_== false
           }
        }
    }

    "student user log in successful with valid request" in {
      testReq("http://test.metl.edu/test/login?ticket=foo", "/test") {

        req => {
         val httpClient = testClient

         val response = new BasicHttpResponse(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK")
         val authenticatedCasResponse = <cas:serviceResponse>
                                           <cas:authenticationSuccess>
                                              <cas:user>jpjor1</cas:user>
                                           </cas:authenticationSuccess>
                                        </cas:serviceResponse>
         response.setEntity(new StringEntity(authenticatedCasResponse.toString))
         response.setStatusCode(HttpStatus.SC_OK)

         when(httpClient.conn.receiveResponseHeader).thenReturn(response)

         val auth = new CASAuthenticator("realm", Some(httpClient.client), () => false, (cs:CASStateData) => { })

         auth.checkReqForCASCookies(req) must_== true
       }
      }
    }

    "user log in successful with valid request" in {
      testReq("http://test.metl.edu/test/login?ticket=foo", "/test") {

        req => {
         //TestVar(CASStateData(true, "Harry", List(("ou", "Unrestricted"), ("uid", "AuthenticatedHarry"), ("ou", "Staff")), 
         // List(("givenname", "Harry"), ("sn", "Henderson"),("mail", "harry.henderson@monash.edu"))))

         val httpClient = testClient

         val response = new BasicHttpResponse(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK")
         val authenticatedCasResponse = <cas:serviceResponse>
                                           <cas:authenticationSuccess>
                                              <cas:user>eecrole</cas:user>
                                           </cas:authenticationSuccess>
                                        </cas:serviceResponse>
         response.setEntity(new StringEntity(authenticatedCasResponse.toString))
         response.setStatusCode(HttpStatus.SC_OK)

         when(httpClient.conn.receiveResponseHeader).thenReturn(response)

         val auth = new CASAuthenticator("realm", Some(httpClient.client), () => false, (cs:CASStateData) => { })

         auth.checkReqForCASCookies(req) must_== true
       }
      }
    }

    "user log in unsuccessful with valid request" in {
      testReq("http://test.metl.edu/test/login?ticket=foo", "/test") {

        req => {
         val httpClient = testClient

         val response = new BasicHttpResponse(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK")
         val authenticatedCasResponse = <cas:serviceResponse>
                                           <cas:authenticationFailure>
                                              <cas:reason>Unknown authcate</cas:reason>
                                           </cas:authenticationFailure>
                                        </cas:serviceResponse>
         response.setEntity(new StringEntity(authenticatedCasResponse.toString))
         response.setStatusCode(HttpStatus.SC_OK)

         when(httpClient.conn.receiveResponseHeader).thenReturn(response)

         val auth = new CASAuthenticator("realm", Some(httpClient.client), () => false, (cs:CASStateData) => { })

         auth.checkReqForCASCookies(req) must_== false
       }
      }
    }
    /*private val testSession = MockWeb.testS("http://test.metl.edu/test/") {
        S.session
    }*/

    /*"properly set up a Req with a HttpServletRequest" withReqFor(testReq) in {
        _.uri must_== "/this"
    }*/

    //private val mockLiftRules = new LiftRules()

    /*"attach to an authenticator" in {
        LiftRulesMocker.devTestLiftRulesInstance.doWith(mockLiftRules) {
            val auth = new CASAuthenticator("realm",() => { println("alreadyLoggedIn"); false }, (cs:CASStateData) => 
            {
                println("loginHandler")
                //Globals.casState(cs)
                //Globals.currentUser(cs.username)
            })

            CASAuthentication.attachCASAuthenticator(auth)
        }
    }*/

    /*"properly set a plain text body" withReqFor("http://test.metl/test/casauthentication") withPost("This is a test") in {
        val auth = new CASAuthenticator("realm",() => { println("alreadyLoggedIn"); false }, (cs:CASStateData) => 
        {
            println("loginHandler")
            //Globals.casState(cs)
            //Globals.currentUser(cs.username)
        })

        CASAuthentication.attachCASAuthenticator(auth)

        req =>
            req.contentType must_== Full("text/plain")
            req.post_? must_== true
            req.body match {
                case Full(body) => (new String(body)) must_== "This is a test"
                case _ => fail("No body set")
            }
    }*/
}
