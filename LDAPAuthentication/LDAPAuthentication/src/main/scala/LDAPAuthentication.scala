package com.metl.ldapAuthenticator

import com.metl.liftAuthenticator._
import com.metl.utils._
import com.metl.ldap._

import scala.xml._
import net.liftweb._
import http._
import js.JsCmds._
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
}

class LDAPAuthenticator(loginPage:NodeSeq, usernameSelector:String, passwordSelector:String, submitButtonSelector:String, ldap: Option[IMeTLLDAP], alreadyLoggedIn:() => Boolean,onSuccess:(LiftAuthStateData) => Unit) extends LiftAuthenticator(alreadyLoggedIn,onSuccess) {

  private val getLDAP: IMeTLLDAP = ldap.getOrElse(DisconnectedLDAP)

  override def checkWhetherAlreadyLoggedIn:Boolean = Stopwatch.time("LDAPAuthenticator.checkWhetherAlreadyLoggedIn", () => alreadyLoggedIn() || InSessionLiftAuthState.is.authenticated)

  def tryLogin(username:String,password:String):Boolean = true

  override def constructResponse(req:Req) = Stopwatch.time("CASAuthenticator.constructReq",() => {
      var username = ""
      var password = ""
      val loginPageNode = (
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
      ).apply(loginPage)
      LiftRules.convertResponse((loginPageNode,List.empty[Tuple2[String,String]],List.empty[HTTPCookie],req))
  })
}
