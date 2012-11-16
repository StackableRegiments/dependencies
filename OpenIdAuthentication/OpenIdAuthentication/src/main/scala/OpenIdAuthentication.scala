package com.metl.auth

import _root_.org.openid4java.discovery._
import _root_.org.openid4java.message.AuthRequest
import _root_.org.openid4java.consumer._
import net.liftweb._
import net.liftweb.util._
import net.liftweb.openid._
import net.liftweb.common._
import net.liftweb.http._
import com.metl.cas._
import net.liftweb.http.SHtml._
import WellKnownAttributes._
import net.liftweb.http.js.JsCmds._
import scala.xml._
import net.liftweb.http.provider.servlet._

case class OpenIdEndpoint(name:String,generateUrlWithUsername:String => String,imageSrc:String,usernameDescriptor:Box[String])

object OpenIdEndpoint {
	private var endpoints = Map.empty[String,OpenIdEndpoint]
	def add(endpoint:OpenIdEndpoint):Unit = endpoints = endpoints.updated(endpoint.name.toLowerCase.trim,endpoint)
	def remove(name:String):Unit = endpoints = endpoints.filter(e => e._1 != name.toLowerCase.trim)  
	def find(name:String):Box[OpenIdEndpoint] = endpoints.get(name.toLowerCase.trim)
	def empty:Unit = endpoints = Map.empty[String,OpenIdEndpoint]
	def getAll:List[OpenIdEndpoint] = endpoints.values.toList
	List(
		OpenIdEndpoint("gmail",(s) => "https://www.google.com/accounts/o8/id","http://certainToFail.com",Empty),
	//	OpenIdEndpoint("GoogleProfile",(s) => "http://www.google.com/profiles/%s".format(s),"http://certainToFail.com",Full("google profile name")),
		OpenIdEndpoint("yahoo",(s) => "https://me.yahoo.com","http://certainToFail.com",Empty),
		OpenIdEndpoint("MyOpenID",(s) => "http://%s.myopenid.com".format(s),"http://certainToFail.com",Full("MyOpenID username")),
		//OpenIdEndpoint("aol",(s) => "https://www.aol.com/","http://certainToFail.com",Empty),
		OpenIdEndpoint("AOL",(s) => "http://openid.aol.com/%s".format(s),"http://certainToFail.com",Full("AOL screen name")),
		OpenIdEndpoint("LiveJournal",(s) => "http://%s.livejournal.com/".format(s),"http://certainToFail.com",Full("LiveJournal username")),
		OpenIdEndpoint("Wordpress",(s) => "http://%s.wordpress.com/".format(s),"http://certainToFail.com",Full("Worldpress.com username")),
		OpenIdEndpoint("Blogger",(s) => "http://%s.blogspot.com/".format(s),"http://certainToFail.com",Full("Bloger account")),
		OpenIdEndpoint("Verisign",(s) => "http://%s.pip.verisignlabs.com/".format(s),"http:certainToFail.com",Full("Verisign username")),
		OpenIdEndpoint("ClickPass",(s) => "http://clickpass.com/public/%s".format(s),"http://certainToFail.com",Full("ClickPass username")),
		OpenIdEndpoint("ClaimID",(s) => "http://claimid.com/%s".format(s),"http://certainToFail.com",Full("ClaimID username"))
	).foreach(e => add(e))	
}

object OpenIdAuthenticator {
  def attachOpenIdAuthenticator(enactor:OpenIdAuthenticator) = {
    LiftRules.dispatch.prepend(enactor.prePf)
    LiftRules.dispatch.append(enactor.postPf)
  }
}

class OpenIdAuthenticator(incomingAlreadyLoggedIn:()=>Boolean,onSuccess:CASStateData => Unit) extends OpenIDVendor {
	private object OriginalRequestPath extends SessionVar[Box[String]](Empty)
	private object attemptInProgress extends SessionVar[Boolean](false)
	private object isLoggedIn extends SessionVar[Boolean](false)

	type UserType = Identifier
	type ConsumerType = OpenIDConsumer[UserType]
  def ext(di: DiscoveryInformation, authReq: AuthRequest): Unit = {
    WellKnownEndpoints.findEndpoint(di) map {
      ep =>
        ep.makeAttributeExtension(List(Email, FullName, FirstName, LastName)) foreach {
          ex =>
            authReq.addExtension(ex)
        }
    }
  }
  def alreadyLoggedIn:Boolean = incomingAlreadyLoggedIn()
  override def createAConsumer = new OpenIDConsumer[UserType] {
    beforeAuth = Full(ext _)
  }

  def postPf:LiftRules.DispatchPF = NamedPF("openIdPostLogin") {
    //case r @ Req(PathRoot :: ResponsePath :: _, "", _) =>
    case r @ Req("openid" :: "response" :: _, _, _) =>
      () => {
				println("openId.postPf")
        for (req <- S.request;
             ret <- {
							 println("generating ret from req: %s".format(req))
							 attemptInProgress(false)
               val (id, res) = OpenIDObject.is.verifyResponse(req.request)
							 println("verifiedResponse: %s".format((id,res)))
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
                   val rb = OriginalRequestPath.is
									 println("redirecting back to: %s".format(rb))
                   Full(RedirectResponse(rb openOr "/", S responseCookies :_*))
                 }
               }
             }) yield ret
      }
  }

  def prePf:LiftRules.DispatchPF = NamedPF("openIdPreLogin") {
		case Req("openIdGoTo" :: choice :: usernameString :: Nil,_,_) => () => {
			OpenIdEndpoint.find(choice).map(we => {
				attemptInProgress(true)
				OpenIDObject.is.authRequest(we.generateUrlWithUsername(usernameString),"/%s/%s".format(PathRoot,ResponsePath))
			})
		}
    case r@Req("openIdGoTo" :: choice :: Nil,_,_) => () => {
			attemptInProgress(false)
			OpenIdEndpoint.find(choice).map(we => {
				we.usernameDescriptor match {
					case Full(argDesc) => {
						val pathName = "/openIdGoTo/"+choice
						val usernameParamName = "username"
						r.param(usernameParamName) match {
							case Full(u) => {
								attemptInProgress(true)
								OpenIDObject.is.authRequest(we.generateUrlWithUsername(u), "/"+PathRoot+"/"+ResponsePath)
							}
							case _ => {
								val xml = <html xmlns="http://www.w3.org/1999/xhtml">
									<body>
										<div>You are authenticating via openId.</div>
										<div>{we.name} needs a {argDesc} to continue.</div>
										<div>Please provide your {argDesc}.</div>
										<form action={pathName} method="post" accept-charset="utf-8">
											<input name={usernameParamName} type="text"/>
											<input type="submit" value="submit"/>
										</form>
									</body>
								</html>
								XhtmlResponse(xml,Empty,Nil,Nil,200,true)
							}
						}	
					}
					case _ => {
						attemptInProgress(true)
						OpenIDObject.is.authRequest(we.generateUrlWithUsername(""), "/"+PathRoot+"/"+ResponsePath)
					}
				}
      })
    }
    case r:Req if (!attemptInProgress.is && !isLoggedIn.is && !alreadyLoggedIn) => () => {
      OriginalRequestPath(Full(r.hostAndPath))
      Full(XhtmlResponse(
				<html xmlns="http://www.w3.org/1999/xhtml">
					<body>
						<div>
						{
							OpenIdEndpoint.getAll.map(we => {
								a(<span><div><img alt={we.name} src={we.imageSrc}/></div><div>{we.name}</div></span>,RedirectTo("/openIdGoTo/"+we.name))
							})
						}</div>
					</body>
				</html>
      ,Empty,Nil,Nil,200,true))
    }
  }
	override def logUserOut:Unit = {}
	override def displayUser(id:OpenIdAuthenticator.this.UserType):NodeSeq = NodeSeq.Empty
	override def currentUser:Box[OpenIdAuthenticator.this.UserType] = Empty
	override def postLogin(id:Box[Identifier], res:VerificationResult):Unit = {}
}
