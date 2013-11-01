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
  override def dispatchTableItemFilter = (r) => false
  protected def dispatchTableItemFilterInternal:Req=>Boolean = (r) => !mod.checkWhetherAlreadyLoggedIn
  override def dispatchTableItem(r:Req) = Full(mod.constructResponse(r))
  LiftRules.dispatch.prepend {
    case r@Req("ldapLogon" :: Nil,_,_) if dispatchTableItemFilterInternal(r) && r.post_? => () => {
      println("received post: %s".format(r))
      r.body.map(b => {
        
        val path = 
        Full(RedirectResponse(path))
      })
      val username = S.param("username")
      val password = S.param("password")
      val path = S.param("path")      
      mod.tryLogin(username,password)
      Full(RedirectResponse(path))
    }
    case r:Req if dispatchTableItemFilterInternal(r) => () => {
      dispatchTableItem(r)
    }
  }
}

class LDAPAuthenticator(loginPage:NodeSeq, formSelector:String, usernameSelector:String, passwordSelector:String, ldap: Option[IMeTLLDAP], alreadyLoggedIn:() => Boolean,onSuccess:(LiftAuthStateData) => Unit) extends LiftAuthenticator(alreadyLoggedIn,onSuccess) {

  private val getLDAP: IMeTLLDAP = ldap.getOrElse(DisconnectedLDAP)

  override def checkWhetherAlreadyLoggedIn:Boolean = Stopwatch.time("LDAPAuthenticator.checkWhetherAlreadyLoggedIn", () => alreadyLoggedIn() || InSessionLiftAuthState.is.authenticated)

  def tryLogin(username:String,password:String):Boolean = {
    println("LDAP.tryLogin")
    onSuccess(LiftAuthStateData(true,username,List.empty[Tuple2[String,String]],List.empty[Tuple2[String,String]]))
    true
  }
 
  override def constructResponse(req:Req) = Stopwatch.time("LDAPAuthenticator.constructReq",() => {
      println("constructing response: %s".format(req))
      var username = ""
      var password = ""
      val loginPageNode =
        (
          formSelector #> {(formNode:NodeSeq) => {
            (
              "%s [method]".format(formSelector) #> "POST" &
              "%s [action]".format(formSelector) #> "/ldapLogon" &
// these next two lines aren't working, and I'm not sure why not
              "%s [name]".format(usernameSelector) #> "username" &
              "%s [name]".format(passwordSelector) #> "password" &
              "%s *+".format(formSelector) #> <input type="hidden" name="path" value={req.uri}></input> &
              "%s *+".format(formSelector) #> <input type="submit" value="Logon"></input>
            ).apply(formNode) 
          }} 
        ).apply(loginPage)
      LiftRules.convertResponse(
        (loginPageNode,200),
        S.getHeaders(LiftRules.defaultHeaders((loginPageNode,req))),
        S.responseCookies,
        req
      )
  })
}
