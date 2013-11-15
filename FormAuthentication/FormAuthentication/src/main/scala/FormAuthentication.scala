package com.metl.formAuthenticator

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

import java.net.URLEncoder

class FormAuthenticationSystem(mod:FormAuthenticator) extends LiftAuthenticationSystem {
  override def dispatchTableItemFilter = (r) => false
  protected def dispatchTableItemFilterInternal:Req=>Boolean = (r) => !mod.checkWhetherAlreadyLoggedIn
  override def dispatchTableItem(r:Req) = Full(mod.constructResponse(r))
  LiftRules.dispatch.prepend {
    case r@Req("formLogon" :: Nil,_,_) if dispatchTableItemFilterInternal(r) && r.post_? => () => {
      val username = S.param("username")
      val password = S.param("password")
      val path = S.param("path")      
      mod.tryLogin(username,password) match {
        case Left(e) => {
          Full(mod.constructResponseWithMessages(r,List(e.getMessage)))
        }
        case Right(true) => {
          Full(RedirectResponse(path.openOr(r.uri)))
        }
        case Right(false) => {
          Full(mod.constructResponseWithMessages(r,List("authentication failure.  Please check your credentials and try again")))
        }
        case _ => {
          Full(mod.constructResponseWithMessages(r,List("unknown error")))
        }
      }
    }
    case r:Req if dispatchTableItemFilterInternal(r) => () => {
      dispatchTableItem(r)
    }
  }
}

class FormAuthenticator(loginPage:NodeSeq, formSelector:String, usernameSelector:String, passwordSelector:String, verifyCredentials:Tuple2[String,String]=>LiftAuthStateData, alreadyLoggedIn:() => Boolean,onSuccess:(LiftAuthStateData) => Unit) extends LiftAuthenticator(alreadyLoggedIn,onSuccess) {

  override def checkWhetherAlreadyLoggedIn:Boolean = Stopwatch.time("FormAuthenticator.checkWhetherAlreadyLoggedIn", () => alreadyLoggedIn() || InSessionLiftAuthState.is.authenticated)

  def tryLogin(username:Box[String],password:Box[String]):Either[Throwable,Boolean] = {
    val response = for (
      u <- username;
      p <- password
    ) yield {
      try {
        val result = verifyCredentials(u,p)
        InSessionLiftAuthState(result)
        onSuccess(result)
        Right(result.authenticated)
      } catch {
        case e:Throwable => {
          Left(e)
        }
      }
    }
    response.openOr(Left(new Exception("username or password not specified")))
  }
  protected def makeUrlFromReq(req:Req):String = {
    val scheme = req.request.scheme
    val url = req.request.serverName
    val port = req.request.serverPort
    val path =req.path.wholePath.mkString("/")
    val newParams = req.params.toList.sortBy(_._1).foldLeft("")((acc,param) => param match { 
      case Tuple2(paramName,listOfParams) => {
        val newParams = listOfParams.map(paramValue => {
          "%s=%s".format(URLEncoder.encode(paramName,"utf-8"), URLEncoder.encode(paramValue,"utf-8"))
        }).mkString("&")
        acc match {
          case "" => acc+"?"+newParams
          case _ => acc+"&"+newParams
        } 
      }
      case _ => acc
    })
    val result = "%s://%s:%s/%s%s".format(scheme,url,port,path,newParams)
    println("made new uri from req: %s".format(result))
    println("req.uri: %s".format(req.uri))
    println("req.request.uri: %s".format(req.request.uri))
    println("req.request.queryString: %s".format(req.request.queryString))
    result
    val newResult = "%s%s".format(req.uri,req.request.queryString.map(qs => "?%s".format(qs)).openOr("")) 
    println("made new uri from req: %s".format(newResult))
    newResult
  }
  override def constructResponse(req:Req) = constructResponseWithMessages(req,List.empty[String]) 
  def constructResponseWithMessages(req:Req,additionalMessages:List[String] = List.empty[String]) = Stopwatch.time("FormAuthenticator.constructReq",() => {
      val loginPageNode =
        (
          formSelector #> {(formNode:NodeSeq) => {
            (
              "%s -*".format(formSelector) #> <input type="hidden" name="path" value={makeUrlFromReq(req)}></input> &
              "%s -*".format(formSelector) #> additionalMessages.map(am => {
                <div class="loginError">{am}</div>
              }) &
              "%s [method]".format(formSelector) #> "POST" &
              "%s [action]".format(formSelector) #> "/formLogon" &
// these next two lines aren't working, and I'm not sure why not
              "%s [name]".format(usernameSelector) #> "username" &
              "%s [name]".format(passwordSelector) #> "password"
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