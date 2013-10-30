package com.metl.cas

import com.metl.liftAuthenticator._
import com.metl.utils._
import com.metl.ldap._

import net.liftweb.http._
import net.liftweb.common._
import scala.collection.immutable.List
import net.liftweb.http.provider.HTTPCookie
import java.net.URLEncoder
import org.apache.commons.io.IOUtils

class CASAuthenticationSystem(mod:CASAuthenticator) extends LiftAuthenticationSystem {
  override def dispatchTableItemFilter = (r) => ((!mod.checkWhetherAlreadyLoggedIn) && (!mod.checkReqForCASCookies(r)))
  override def dispatchTableItem(r:Req) = Full(mod.constructResponse(r))
}

class CASAuthenticator(realm:String, httpClient: Option[IMeTLHttpClient], ldap: Option[IMeTLLDAP], alreadyLoggedIn:() => Boolean,onSuccess:(LiftAuthStateData) => Unit) extends LiftAuthenticator(alreadyLoggedIn,onSuccess) {

  def this(realm: String, alreadyLoggedIn: () => Boolean, onSuccess: (LiftAuthStateData) => Unit) {
      this(realm, None, None, alreadyLoggedIn, onSuccess)
  }

  private def getHttpClient: IMeTLHttpClient = httpClient.getOrElse(Http.getClient)
  private val getLDAP: IMeTLLDAP = ldap.getOrElse(DisconnectedLDAP)

  override def checkWhetherAlreadyLoggedIn:Boolean = Stopwatch.time("CASAuthenticator.checkWhetherAlreadyLoggedIn", () => alreadyLoggedIn() || InSessionLiftAuthState.is.authenticated)

  val monashCasUrl = "https://my.monash.edu.au/authentication/cas"
  def checkReqForCASCookies(req:Req):Boolean = Stopwatch.time("CASAuthenticator.checkReqForCASCookies", () => {
    val result = verifyCASTicket(req)
    if (result.authenticated) {
      InSessionLiftAuthState.set(result)
      onSuccess(result)
      true
    } else {
      false
    }
  })
  private def ticketlessUrl(originalRequest : Req):String = Stopwatch.time("CASAuthenticator.ticketlessUrl", () => {
    val url = originalRequest.request.serverName
    val port = originalRequest.request.serverPort
    val path = originalRequest.path.wholePath.mkString("/")
    val newParams = originalRequest.params.toList.sortBy(_._1).foldLeft("")((acc, param) => param match {
       case Tuple2(paramName,listOfParams) if (paramName.toLowerCase == "ticket") => acc
       case Tuple2(paramName,listOfParams) => {
        val newItem = "%s=%s".format(URLEncoder.encode(paramName, "utf-8"), URLEncoder.encode(listOfParams.mkString(""), "utf-8")) 
        acc match {
          case "" => acc+"?"+newItem
          case _ => acc+"&"+newItem
        }
       }
       case _ => acc
     })
   newParams.length match {
     case 0 => "%s://%s:%s/%s".format(originalRequest.request.scheme,url,port,path)
     case _ => "%s://%s:%s/%s%s".format(originalRequest.request.scheme,url,port,path,newParams)
   }
  })
  private def verifyCASTicket(req:Req) : LiftAuthStateData = Stopwatch.time("CASAuthenticator.verifyCASTicket", () => {
      req.param("ticket") match {
      case Full(ticket) =>
      {
        val verifyUrl = monashCasUrl + "/serviceValidate?ticket=%s&service=%s"
        val casValidityResponse = getHttpClient.getAsString(verifyUrl.format(ticket,URLEncoder.encode(ticketlessUrl(req), "utf-8")))
        val casValidityResponseXml = xml.XML.loadString(casValidityResponse)
        val state = for(success <- (casValidityResponseXml \\ "authenticationSuccess");
          user <- (success \\ "user");
          groups <- getLDAP.ou(List(user.text)).get(user.text);
          info <- getLDAP.info(List(user.text)).get(user.text)
        ) yield LiftAuthStateData(true,user.text,groups,info)
        state match{
          case newState :: Nil if newState.authenticated == true => newState
          case _ => LiftAuthStateDataForbidden
        }
      }
      case Empty => LiftAuthStateDataForbidden
      case _ => LiftAuthStateDataForbidden
    }
  })
  val redirectUrl = monashCasUrl + "/login/?service=%s" 
  override def constructResponse(req:Req) = Stopwatch.time("CASAuthenticator.constructReq",() => {
      val url = redirectUrl.format(URLEncoder.encode(ticketlessUrl(req),"utf-8"))
      new RedirectResponse(url, req)
  })
}
