package com.metl.ldapAuthenticator

import com.metl.liftAuthenticator._
import com.metl.formAuthenticator._
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

class LDAPAuthenticationSystem(mod:LDAPAuthenticator) extends FormAuthenticationSystem(mod) {
}

class LDAPAuthenticator(loginPage:NodeSeq, formSelector:String, usernameSelector:String, passwordSelector:String, ldap: Option[IMeTLLDAP], alreadyLoggedIn:() => Boolean,onSuccess:(LiftAuthStateData) => Unit) extends FormAuthenticator(loginPage,formSelector,usernameSelector,passwordSelector,c => ldap.map(l => {
  println("trying to authenticate with LDAP authenticator")
  val u = c._1
  val p = c._2
  l.authenticate(u,p) match {
    case Some(true) => {
      println("successful auth")
      LiftAuthStateData(true,u,l.getEligibleGroups(u).getOrElse(List.empty[Tuple2[String,String]]),l.getInformationGroups(u).getOrElse(List.empty[Tuple2[String,String]]))
    }
    case _ => {
      println("failed auth")
      LiftAuthStateDataForbidden
    }
  }
}).getOrElse({
  println("no LDAP connector provided")
  LiftAuthStateDataForbidden
}),alreadyLoggedIn,onSuccess) {
}
