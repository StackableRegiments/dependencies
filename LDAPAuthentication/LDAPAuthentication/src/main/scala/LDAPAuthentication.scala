package com.metl.ldapAuthenticator

import com.metl.liftAuthenticator._
import com.metl.utils._
import com.metl.ldap._

import scala.xml._
import net.liftweb._
import http._
import js._
import JsCmds._
import util._
import Helpers._
import common._
import SHtml._
import scala.collection.immutable.List
import net.liftweb.http.provider.HTTPCookie
import org.apache.commons.io.IOUtils

class LDAPAuthenticationSystem(mod:LDAPAuthenticator) extends LiftAuthenticationSystem {
  override def dispatchTableItemFilter = (r) => !mod.checkWhetherAlreadyLoggedIn
  override def dispatchTableItem(r:Req) = Full(mod.constructResponse(r))
/*  LiftRules.dispatch.prepend {
    case r@Req("ldapLogon" :: Nil,_,_) if dispatchTableItemFilter(r) && r.post_? => () => {
      
    }
  }
*/
}

class LDAPAuthenticator(loginPage:NodeSeq, usernameSelector:String, passwordSelector:String, submitButtonSelector:String, ldap: Option[IMeTLLDAP], alreadyLoggedIn:() => Boolean,onSuccess:(LiftAuthStateData) => Unit) extends LiftAuthenticator(alreadyLoggedIn,onSuccess) {

  private val getLDAP: IMeTLLDAP = ldap.getOrElse(DisconnectedLDAP)

  override def checkWhetherAlreadyLoggedIn:Boolean = Stopwatch.time("LDAPAuthenticator.checkWhetherAlreadyLoggedIn", () => alreadyLoggedIn() || InSessionLiftAuthState.is.authenticated)

  def tryLogin(username:String,password:String):Boolean = {
    println("LDAP.tryLogin")
    onSuccess(LiftAuthStateDataForbidden)
    true
  }
 
  val defaultNodeSeq:NodeSeq = {
    <html xmlns:lift="http://liftweb.net/" xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <meta content="text/html; charset=UTF-8" http-equiv="content-type" />
        <title>LDAP Login</title>
      </head>
      <body><div>Error</div><div>This system does not have a valid authentication page set up.  It is not possible to log into this system.</div></body>
    </html>
  }

  override def constructResponse(req:Req) = Stopwatch.time("LDAPAuthenticator.constructReq",() => {
      var username = ""
      var password = ""
      val withAddedScript = 
        <html xmlns="http://www.w3.org/1999/xhtml" xmlns:lift="http://liftweb.net/">
          <head>
            <!--<script id="jquery" src="/classpath/jquery.js" type="text/javascript"></script>-->
            <script id="jquery" src="/static/js/jquery-1.7.2.min.js" type="text/javascript"></script>
            <script id="json" src="/classpath/json.js" type="text/javascript"></script>
          </head>
          <body>
            {loginPage}
            {Script(ScriptRenderer.ajaxScript)}
            <div data-lift="logonPage"></div>
          </body>
        </html>
      val loginPageNode = 
        (
          usernameSelector #> ajaxText(username,(t) => {
            username = t
          }) &
          passwordSelector #> ajaxText(password,(t) => {
            password = t
          },("type","password")) &
          submitButtonSelector #> ajaxButton("login",() => {
            if (tryLogin(username,password)){
              RedirectTo(req.uri)
            } else {
              Alert("login error") 
            }
          })
        ).apply(withAddedScript)

      val output = for (
        session <- S.session;
        response <- session.processTemplate(Full(session.processSurroundAndInclude("logonPage",loginPageNode)),req,req.path,200)
      ) yield {
        response
      }
      output.openOr(PlainTextResponse("serverside authentication error",500))   
/*      LiftRules.convertResponse(
        (loginPageNode,200),
        S.getHeaders(LiftRules.defaultHeaders((loginPageNode,req))),
        S.responseCookies,
        req
      )
*/
  })
}
