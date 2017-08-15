package com.metl.external

import net.liftweb._
import util._
import Helpers._
import common._
import http._
import provider._
import servlet._
import javax.servlet.http.HttpServletRequest

import com.metl.ReadOnlyMetlInterface
import org.imsglobal.lti.launch._
import org.imsglobal.pox.IMSPOXRequest
import org.apache.http.client.methods.HttpPost

import scala.xml.NodeSeq

abstract class ExternalLtiConfigurator extends Logger {
  def configureFromXml(in:NodeSeq,metlIn:ReadOnlyMetlInterface,currentUserAccessor:() => String,isDevModeAccessor:() => Boolean):Either[Exception,List[LtiIntegration]]
}

case class LtiUser(id:String,roles:List[String])
case class LtiLaunchResult(success:Boolean, message:String, result:Either[Exception,LtiLaunch])
case class LtiLaunch(user:LtiUser,version:String,messageType:String,resourceLinkId:String,contextId:String,launchPresentationReturnUrl:String,toolConsumerInstanceGuid:String)

class RemotePluginSession(val token:String,val secret:String,val key:String,val launch:LtiLaunchResult)

abstract class LtiIntegration(ltiIntegrations:Seq[(String,String)],metl:ReadOnlyMetlInterface,currentUserAccessor:() => String,isDevModeAccessor:() => Boolean) extends Logger {

  object sessionStore extends SessionVar[Map[String,RemotePluginSession]](Map.empty[String,RemotePluginSession])
  if (isDevModeAccessor()) {
    sessionStore(sessionStore.is.updated("testToken",new RemotePluginSession("testToken",
      "testSecret",
      "testKey",
      LtiLaunchResult(success = true,"test message",Right(LtiLaunch(LtiUser(currentUserAccessor(),List("testUser")),"v0.test","testMessageType","testResourceLinkId","testContextId",metl.noBoard,"testGuid"))))))
  }
  val consumerKeyParamName = "oauth_consumer_key"
  val secretMap = Map(ltiIntegrations:_*)
  def getSecretForKey(key:String):String = {
    val secret = secretMap.get(key).getOrElse("secret")
    secret
  }

  def sendScore(url:String,key:String,sourcedid:String,score:String,resultData:String):Either[Exception,Unit] = {
    val secret = getSecretForKey(key)
    val postAction:HttpPost = IMSPOXRequest.buildReplaceResult(url,key,secret,sourcedid,score,resultData,true)
    Left(new Exception("not yet implemented"))
  }

  def verifyLtiLaunch(reqBox:Box[HTTPRequest] = S.containerRequest):Either[Exception,RemotePluginSession] = {
    try {
      reqBox match {
        case Full(req) => req match {
          case hrs:HTTPRequestServlet => {
            val cReq:HttpServletRequest = hrs.req
            val verifier = new LtiOauthVerifier()
            req.param(consumerKeyParamName) match {
              case key :: Nil => {
                val secret = getSecretForKey(key)
                val result:LtiVerificationResult = verifier.verify(cReq,secret)
                val token = nextFuncName
                Right(new RemotePluginSession(token,secret,key,LtiLaunchResult(result.getSuccess,result.getMessage,(result.getError,result.getLtiLaunchResult) match {
                  case (err,res) if res != null && res.getUser != null && res.getUser.getId != null && res.getUser.getId != "" => {
                    Right(LtiLaunch(LtiUser(res.getUser.getId,res.getUser.getRoles.toArray.toList.map(_.toString)),res.getVersion,res.getMessageType,res.getResourceLinkId,res.getContextId,res.getLaunchPresentationReturnUrl,res.getToolConsumerInstanceGuid))
                  }
                  case (err,_) => Left(new Exception(err.toString))
                })))
              }
              case notAKey => {
                Left(new Exception("request parameter not found: %s => %s".format(consumerKeyParamName,notAKey)))
              }
            }
          }
          case notAHttpReq => {
            Left(new Exception("servlet request is not a httpServletRequest: %s".format(notAHttpReq)))
          }
        }
        case noReq => {
          Left(new Exception("request not available: %s".format(noReq)))
        }
      }
    } catch {
      case e:Exception => Left(e)
    }
  }
  def handleLtiRequest(in:Req,onSuccess:RemotePluginSession=>Box[LiftResponse],storeSession:Boolean = true):Box[LiftResponse] = {
    verifyLtiLaunch(Full(in).map(_.request)) match {
      case Left(e) => {
        error("error while parsing lti request",e)
        Failure(e.getMessage,Full(e),Empty)
      }
      case Right(pluginSession) => {
        if (storeSession){
          sessionStore(sessionStore.updated(pluginSession.token,pluginSession))
        }
        onSuccess(pluginSession)
      }
    }
  }
  def generateContentResponse(returnUrl:String,htmlContent:String):LiftResponse
  def generateQuickLinkResponse(returnUrl:String,url:String,title:String,target:String):LiftResponse
  def generateResponse(returnUrl:String):LiftResponse
  def insertConversationIFrame(ltiToken:String,conversationJid:String):Unit
  def insertConversationQuickLink(ltiToken:String,conversationJid:String):Unit
  def insertConversationSlideIFrame(ltiToken:String,conversationJid:String,slideJid:String):Unit
  def insertConversationSlideQuickLink(ltiToken:String,conversationJid:String,slideJid:String):Unit
}

