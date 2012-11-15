package com.metl.auth

import _root_.org.openid4java.discovery._
import _root_.org.openid4java.message.AuthRequest
import _root_.org.openid4java.consumer._
import net.liftweb.openid._
import net.liftweb.common._
import net.liftweb.http._
import com.metl.cas._
import net.liftweb.http.SHtml._
import WellKnownAttributes._

object isLoggedIn extends SessionVar[Boolean](false)

object OpenIdAuthenticator {
  def attachOpenIdAuthenticator(enactor:OpenIdAuthenticator) = {
    LiftRules.dispatch.prepend(enactor.prePf)
    LiftRules.dispatch.append(enactor.postPf)
  }
}

class OpenIdAuthenticator(alreadyLoggedIn:()=>Boolean,onSuccess:CASStateData => Unit) extends OpenIdVendor {
  def ext(di: DiscoveryInformation, authReq: AuthRequest): Unit = {
    WellKnownEndpoints.findEndpoint(di) map {
      ep =>
        ep.makeAttributeExtension(List(Email, FullName, FirstName, LastName)) foreach {
          ex =>
            authReq.addExtension(ex)
        }
    }
  }
  def alreadyLoggedIn = alreadyLoggedIn
  override def createAConsumer = new OpenIDConsumer[UserType] {
    beforeAuth = Full(ext _)
  }

  def postPf = {
    case r @ Req(PathRoot :: ResponsePath :: _, "", _) =>
      () => {
        for (req <- S.request;
             ret <- {
               val (id, res) = OpenIDObject.is.verifyResponse(req.request)
               id.map(i=>{
                 val attrs = WellKnownAttributes.attributeValues(res.getAuthResponse)
                 println("attrs")
                 attrs.map(println)
                 for(first <- attrs.get(FirstName);
                     last <- attrs.get(LastName)) yield "%s%s".format(first,last) match {
                       case name =>{
                         println("identified ",name)
                         val casData = CASStateData(true,name,List(("ou","sandbox")),List(("FirstName",first),("LastName",last)))
                         isLoggedIn(true)
                         onSuccess(casData)
                       }
                     }
               })
               OpenIDObject.onComplete match {
                 case Full(f) => Full(f(id, Full(res), Empty))
                 case _ => {
                   postLogin(id, res)
                   val rb = RedirectBackTo.is
                   Full(RedirectResponse(rb openOr "/", S responseCookies :_*))
                 }
               }
             }) yield ret
      }
  }

  def prePf = {
    case Req("openIdGoTo" :: choice :: paramName,_,_) => () => {
      WellknownEndpoints.find(we => we.name == choice).map(we => {
        OpenIDObject.is.authRequest(we.url, "/"+PathRoot+"/"+ResponsePath)
      })
    }
    case r:Req if (!isLoggedIn.is && !alreadyLoggedIn())=> {
      RedirectBackTo(r.hostAndPath)
      Full(InMemoryResponse(
        <div>
        {
          WellknownEndpoints.map(we => {
            a(<img alt={we.name} src={"http://certainToFail.com"}/>,() => RedirectTo("/openIdGoTo/"+we.name))
          })
        }</div>
      ))
    }
  }
}
