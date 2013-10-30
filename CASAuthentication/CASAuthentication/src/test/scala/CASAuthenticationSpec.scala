package com.metl.cas

import com.metl.liftAuthenticator._
import com.metl.ldap._
import com.metl.utils._
import javax.servlet.http.HttpServletRequest

import net.liftweb.common.{Box, Full, Empty}
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.mockweb.MockWeb
import net.liftweb.mockweb.WebSpec
import net.liftweb.http.{LiftRules, LiftRulesMocker, S, SessionVar, RedirectResponse, ForbiddenResponse}

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

    val testUrl = "http://test.metl.edu/test/this"
    def testSession = MockWeb.testS(testUrl) {
      S.session
    }

    val testUrlWithParams = "http://test.metl.edu/test/this?testing=true&foo=hbar"
    def testSessionWithParams = MockWeb.testS(testUrlWithParams) {
      S.session
    }

    case class F(connMgr: ClientConnectionManager, client: CleanHttpClient, conn: ManagedClientConnection, connRequest: ClientConnectionRequest)
    private def testClient = {

        val connMgr = mock[ClientConnectionManager]
        val client = new CleanHttpClient(connMgr)
        val conn = mock[ManagedClientConnection]
        val connRequest = mock[ClientConnectionRequest]

        when(connRequest.getConnection(anyInt, any(classOf[TimeUnit]))).thenReturn(conn)
        when(connMgr.requestConnection(any(classOf[HttpRoute]), any)).thenReturn(connRequest)
        when(conn.isResponseAvailable(anyInt)).thenReturn(true)

        F(connMgr, client, conn, connRequest)
    }

    private def testLDAP = {
        mock[IMeTLLDAP]
    }

    private def buildLDAPOutput(names: Seq[String], data: Seq[Tuple2[String,String]]) : Map[String, Seq[(String,String)]] = {
        Map(names.map{n=> (n,data)} : _*)
    }

    "create a CASAuthenticator" in {

        var onSuccess = false
        val auth = new CASAuthenticator("realm", () => false, (cs:LiftAuthStateData) => 
        {
            onSuccess = true
        })

        auth must notBeNull
        onSuccess mustBe false
    }

    "check whether already logged in returns false when not logged in" in {

        var onSuccess = false
        val auth = new CASAuthenticator("realm",() => false, (cs:LiftAuthStateData) => 
        {
            onSuccess = true
        })

        auth.checkWhetherAlreadyLoggedIn mustBe false
        onSuccess mustBe false
    }

    "user login unsuccessful with invalid request" withSFor(testUrl, testSession) in {
        testReq("http://test.metl.edu/test/this?foo=hbar", "/test") {

            req => {

             val httpClient = testClient
             var onSuccess = false
             val auth = new CASAuthenticator("realm", Some(httpClient.client), Some(testLDAP), () => false, (cs:LiftAuthStateData) => 
             { 
               onSuccess = true
             })

             auth.checkReqForCASCookies(req) mustBe false
             auth.checkWhetherAlreadyLoggedIn mustBe false
             onSuccess mustBe false
           }
        }
    }

    "student user login successful with valid request" withSFor(testUrl, testSession) in {
      testReq("http://test.metl.edu/test/login?ticket=foo", "/test") {

        req => {
           val httpClient = testClient
           val ldap = testLDAP

           val user = Seq("test1")
           val ou = List(("ou","Unrestricted"), ("uid","test1"), ("ou","Student"), ("monashenrolledsubject","mth2021"), 
             ("monashenrolledsubject","phs2011"), ("monashenrolledsubject","mth2032"), ("monashenrolledsubject","phs2022"), 
             ("monashenrolledsubject","mth2010"), ("monashenrolledsubject","fit2002"))

           val info = List(("sn","Tester"), ("givenname","Testy Test"), ("givenname","Testy"), ("initials","T T"), 
             ("mail","test1@student.monash.edu"), ("cn","Testy Test Tester"), ("cn","Testy Tester"), ("gender","Male"), ("personaltitle","Mr"), ("employeenumber","21444480"))

           when(ldap.ou(user)).thenReturn(buildLDAPOutput(user, ou))
           when(ldap.info(user)).thenReturn(buildLDAPOutput(user, info))

           val response = new BasicHttpResponse(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK")
           val authenticatedCasResponse = <cas:serviceResponse>
                                             <cas:authenticationSuccess>
                                                <cas:user>test1</cas:user>
                                             </cas:authenticationSuccess>
                                          </cas:serviceResponse>
           response.setEntity(new StringEntity(authenticatedCasResponse.toString))
           response.setStatusCode(HttpStatus.SC_OK)

           when(httpClient.conn.receiveResponseHeader).thenReturn(response)

           var onSuccess = false
           val auth = new CASAuthenticator("realm", Some(httpClient.client), Some(ldap), () => false, (cs:LiftAuthStateData) => { onSuccess = true })

           auth.checkReqForCASCookies(req) mustBe true
           auth.checkWhetherAlreadyLoggedIn mustBe true
           onSuccess mustBe true
       }
      }
    }

    "user login successful with valid request" withSFor(testUrl, testSession) in {
      testReq("http://test.metl.edu/test/login?ticket=foo", "/test") {

        req => {
         val httpClient = testClient
         val ldap = testLDAP

         val user = Seq("eecrole")
         val ou = List(("monashmetacn","monashmetacn: EEC Role"), ("ou","Unrestricted"), ("uid","eecrole"), ("ou","Staff"), ("ou","Administration"), 
           ("ou", "Office of the Deputy Vice-Chancellor (Education)"))

         val info = List(("sn","Role"), ("givenname","EEC"), ("initials","E"), ("mail","EEC.Role@monash.edu"), ("cn","EEC Role"), ("personaltitle","ROLE"))

         when(ldap.ou(user)).thenReturn(buildLDAPOutput(user, ou))
         when(ldap.info(user)).thenReturn(buildLDAPOutput(user, info))
         
         val response = new BasicHttpResponse(HttpVersion.HTTP_1_1, HttpStatus.SC_OK, "OK")
         val authenticatedCasResponse = <cas:serviceResponse>
                                           <cas:authenticationSuccess>
                                              <cas:user>eecrole</cas:user>
                                           </cas:authenticationSuccess>
                                        </cas:serviceResponse>
         response.setEntity(new StringEntity(authenticatedCasResponse.toString))
         response.setStatusCode(HttpStatus.SC_OK)

         when(httpClient.conn.receiveResponseHeader).thenReturn(response)

         var onSuccess = false
         val auth = new CASAuthenticator("realm", Some(httpClient.client), Some(ldap), () => false, (cs:LiftAuthStateData) => { onSuccess = true })

         auth.checkReqForCASCookies(req) mustBe true
         auth.checkWhetherAlreadyLoggedIn mustBe true
         onSuccess mustBe true
       }
      }
    }

    "user login unsuccessful with valid request" withSFor(testUrl, testSession) in {
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

         var onSuccess = false
         val auth = new CASAuthenticator("realm", Some(httpClient.client), Some(testLDAP), () => false, (cs:LiftAuthStateData) => { onSuccess = true })

         auth.checkReqForCASCookies(req) mustBe false
         auth.checkWhetherAlreadyLoggedIn mustBe false
         onSuccess mustBe false
       }
      }
    }

    "redirect returns expected redirectresponse" withSFor(testUrl, testSession) in {
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

         val auth = new CASAuthenticator("realm", Some(httpClient.client), Some(testLDAP), () => false, (cs:LiftAuthStateData) => { false })

         val redirect = S.request.map(req => auth.constructResponse(req)).openOr(ForbiddenResponse("unknown cas error"))
         redirect must haveClass[RedirectResponse]
         redirect match {
           case r:RedirectResponse => r.uri must beEqualTo("https://my.monash.edu.au/authentication/cas/login/?service=http%3A%2F%2Ftest.metl.edu%3A80%2Ftest%2Fthis")
           case _ => "redirect is a redirect" must beEqualTo("true")
        }
    }

    "redirect returns expected redirectresponse" withSFor(testUrlWithParams, testSessionWithParams) in {
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

         val auth = new CASAuthenticator("realm", Some(httpClient.client), Some(testLDAP), () => false, (cs:LiftAuthStateData) => { false })

         val redirect = S.request.map(req => auth.constructResponse(req)).openOr(ForbiddenResponse("unknown cas error"))
         redirect must haveClass[RedirectResponse]
         redirect match {
           case r:RedirectResponse => r.uri must beEqualTo("https://my.monash.edu.au/authentication/cas/login/?service=http%3A%2F%2Ftest.metl.edu%3A80%2Ftest%2Fthis%3Ffoo%3Dhbar%26testing%3Dtrue")
           case _ => "redirect is a redirect" must beEqualTo("true")
        }
    }
}
