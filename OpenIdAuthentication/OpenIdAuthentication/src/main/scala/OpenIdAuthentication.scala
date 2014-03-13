package com.metl.auth

import _root_.org.openid4java.discovery._
import _root_.org.openid4java.message.AuthRequest
import _root_.org.openid4java.message.ParameterList
import _root_.org.openid4java.consumer._
import net.liftweb._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import net.liftweb.openid._
import net.liftweb.common._
import net.liftweb.http._
import provider._
import com.metl.liftAuthenticator._
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
		OpenIdEndpoint("yahoo",(s) => "https://me.yahoo.com","http://certainToFail.com",Empty),
		OpenIdEndpoint("MyOpenID",(s) => "http://%s.myopenid.com".format(s),"http://certainToFail.com",Full("MyOpenID username")),
		OpenIdEndpoint("AOL",(s) => "http://openid.aol.com/%s".format(s),"http://certainToFail.com",Full("AOL screen name")),
		OpenIdEndpoint("LiveJournal",(s) => "http://%s.livejournal.com/".format(s),"http://certainToFail.com",Full("LiveJournal username")),
		OpenIdEndpoint("Wordpress",(s) => "http://%s.wordpress.com/".format(s),"http://certainToFail.com",Full("Wordpress.com username")),
		OpenIdEndpoint("Blogger",(s) => "http://%s.blogspot.com/".format(s),"http://certainToFail.com",Full("Blogger account")),
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

class OpenIdAuthenticator(incomingAlreadyLoggedIn:()=>Boolean,onSuccess:LiftAuthStateData => Unit,endpoints:Box[List[OpenIdEndpoint]] = Empty) extends OpenIDVendor {
	private object OriginalRequestPath extends SessionVar[Box[String]](Empty)
	private object attemptInProgress extends SessionVar[Boolean](false)
	private object isLoggedIn extends SessionVar[Boolean](false)

  protected val overrideHost:Box[String] = Empty
  protected val overridePort:Box[Int] = Empty
  protected val overrideScheme:Box[String] = Empty

  protected val manager = OpenIDObject.is.manager

  protected def getEndpointByName(eName:String):Box[OpenIdEndpoint] = {
    endpoints.map(es => es.find(e => e.name == eName) match {
      case Some(e) => Full(e)
      case _ => Empty
    }).openOr(OpenIdEndpoint.find(eName))
  }
  protected def getAllEndpoints:List[OpenIdEndpoint] = {
    endpoints.openOr(OpenIdEndpoint.getAll)
  }

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
				//println("openId.postPf")
        for (req <- S.request;
             ret <- {
							 //println("generating ret from req: %s".format(req))
							 attemptInProgress(false)
               //val (id, res) = OpenIDObject.is.verifyResponse(req.request)
               val (id, res) = verifyResponse(req.request)
							 //println("verifiedResponse: %s".format((id,res)))
               id.map(i=>{
                 val attrs = WellKnownAttributes.attributeValues(res.getAuthResponse)
                 //println("attrs")
                 //attrs.map(println)
                 for(first <- attrs.get(FirstName);
                     last <- attrs.get(LastName)) yield "%s%s".format(first,last) match {
                       case name =>{
                         //println("identified ",name)
                         val casData = LiftAuthStateData(true,name,List(("ou","sandbox"),("ou","unrestricted")),List(("FirstName",first),("LastName",last)))
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
									 //println("redirecting back to: %s".format(rb))
                   Full(RedirectResponse(rb openOr "/", S responseCookies :_*))
                 }
               }
             }) yield ret
      }
    case r:Req if attemptInProgress.is && !alreadyLoggedIn => () => {
      attemptInProgress(false)
      isLoggedIn(false)
      Full(RedirectResponse("/", S responseCookies :_*))
    }
  }

  protected def generateHostAndPort(r:Req):String = {
    val originalHost = r.request.serverName
    val originalPort = r.request.serverPort
    val originalScheme = r.request.scheme
    var scheme = overrideScheme.openOr(r.header("X_FORWARDED_PROTO") match {
      case Full(s) if s.length > 0 => s
      case _ => originalScheme
    })
    var port = overridePort.openOr(r.header("X_FORWARDED_PORT") match {
      case Full(s) if s.length > 0 => {
        try {
          s.toInt
        } catch {
          case e:Throwable => originalPort
        }
      }
      case _ => originalPort
    })
    var host = overrideHost.openOr(r.header("X-FORWARDED-HOST") match {
      case Full(hostAndPort) if hostAndPort.length > 0 => {
        hostAndPort.split(":") match {
          case Array(h,p) => {
            try {
              port = p.toInt
            } catch {
              case e:Throwable => {}
            }
            h 
          }
          case Array(h) => h 
          case _ => originalHost
        }
      }
      case _ => originalHost
    })
    "%s://%s:%s".format(scheme,host,port)
  }


  protected def verifyResponse(httpReq: HTTPRequest): (Box[Identifier], VerificationResult) =  
    //this is just so that I can override the original scheme behaviour at the right moment.  This is from http://scala-tools.org/mvnsites/liftweb-2.0/framework/scaladocs/net/liftweb/openid/OpenID.scala.html line 200
  {  
    // extract the parameters from the authentication response  
    // (which comes in as a HTTP request from the OpenID provider)  
    val paramMap = new java.util.HashMap[String, String]  
    httpReq.params.foreach(e => paramMap.put(e.name, e.values.firstOption getOrElse null))  
    val response =  new ParameterList(paramMap);  
  
    // retrieve the previously stored discovery information  
    val discovered = httpReq.session.attribute("openid-disc") match {  
      case d: DiscoveryInformation => d  
      case _ => throw ResponseShortcutException.redirect("/")  
    }  
  
    // extract the receiving URL from the HTTP request  
    //var receivingURL = httpReq.url  
    //modifying the receivingURL to take note of the http overrides
    var receivingURL = httpReq.url  
    val parts = receivingURL.split(":")
    val origScheme = parts.head
    val origHost = parts.drop(1).head
    val remainingParts = parts.drop(2).head.split("/")
    val origPort = remainingParts.head
    val pathParts = remainingParts.drop(1)
    receivingURL = List(List(overrideScheme.openOr(origScheme),overrideHost.map(oh => "//"+oh).openOr(origHost),overridePort.openOr(origPort)).mkString(":"),pathParts.mkString("/")).mkString("/")

    val queryString = httpReq.queryString openOr ""  
    if (queryString != null && queryString.length() > 0) {  
      receivingURL += "?" + queryString;  
    }  
  
    //println("receivingUrl: %s (<= %s)".format(receivingURL,OriginalRequestPath.is))
    
    // verify the response; ConsumerManager needs to be the same  
    // (static) instance used to place the authentication request  
    val verification = manager.verify(receivingURL.toString(),  
                                      response, discovered)  
  
    // examine the verification result and extract the verified identifier  
  
    val verified = verification.getVerifiedId();  
  
    (Box.legacyNullTest(verified), verification)  
  }  

  protected def generateAuthRequest(r:Req,userSuppliedString:String,targetUrl:String):LiftResponse = {
    //this is just so that I can override the original scheme behaviour at the right moment.  This is from http://scala-tools.org/mvnsites/liftweb-2.0/framework/scaladocs/net/liftweb/openid/OpenID.scala.html line 200

    // configure the return_to URL where your application will receive  
    // the authentication responses from the OpenID provider  
    //val returnToUrl = S.encodeURL(S.hostAndPath + targetUrl)  
    val hostAndPort = generateHostAndPort(r)
    
    val returnToUrl = S.encodeURL("%s/%s".format(hostAndPort,targetUrl))  
    //println("returnToUrl: %s".format(returnToUrl)) 
    // perform discovery on the user-supplied identifier  
    val discoveries = manager.discover(userSuppliedString)  
  
    // attempt to associate with the OpenID provider  
    // and retrieve one service endpoint for authentication  
    val discovered = manager.associate(discoveries)  
  
    S.containerSession.foreach(_.setAttribute("openid-disc", discovered))  
  
    // obtain a AuthRequest message to be sent to the OpenID provider  
    val authReq = manager.authenticate(discovered, returnToUrl)  
  
    OpenIDObject.is.beforeAuth.foreach(f => f(discovered, authReq))  
      
    if (! discovered.isVersion2() )  
    {  
      // Option 1: GET HTTP-redirect to the OpenID Provider endpoint  
      // The only method supported in OpenID 1.x  
      // redirect-URL usually limited ~2048 bytes  
      RedirectResponse(authReq.getDestinationUrl(true))  
    }  
    else  
    {  
      // Option 2: HTML FORM Redirection (Allows payloads >2048 bytes)  
      val pm =  authReq.getParameterMap()  
      val info: Seq[(String, String)] = pm.keySet.toArray.  
      map(k => (k.toString, pm.get(k).toString))  
  
      XhtmlResponse(  
        <html xmlns="http://www.w3.org/1999/xhtml">  
          <head>  
            <title>OpenID HTML FORM Redirection</title>  
          </head>  
          <body onload="document.forms['openid-form-redirection'].submit();">  
            <form name="openid-form-redirection" action={authReq.getDestinationUrl(false)} method="post" accept-charset="utf-8">  
              {  
                info.map{ case(key, value) =>  
                    <input type="hidden" name={key} value={value}/>  
                }  
              }  
              <button type="submit">Continue...</button>  
            </form>  
          </body>  
        </html>, Empty, Nil, Nil, 200, true)  
    }  
    
  }

  def prePf:LiftRules.DispatchPF = NamedPF("openIdPreLogin") {
		case r@Req("openIdGoTo" :: choice :: usernameString :: Nil,_,_) => () => {
			getEndpointByName(choice).map(we => {
				attemptInProgress(true)
				generateAuthRequest(r,we.generateUrlWithUsername(usernameString),"%s/%s".format(PathRoot,ResponsePath))
        //OpenIDObject.is.authRequest(we.generateUrlWithUsername(usernameString),"/%s/%s".format(PathRoot,ResponsePath))
			})
		}
    case r@Req("openIdGoTo" :: choice :: Nil,_,_) => () => {
			attemptInProgress(false)
			getEndpointByName(choice).map(we => {
				we.usernameDescriptor match {
					case Full(argDesc) => {
						val pathName = "/openIdGoTo/"+choice
						val usernameParamName = "username"
						r.param(usernameParamName) match {
							case Full(u) => {
								attemptInProgress(true)
                generateAuthRequest(r,we.generateUrlWithUsername(u),"%s/%s".format(PathRoot,ResponsePath))
                //OpenIDObject.is.authRequest(we.generateUrlWithUsername(u), "/"+PathRoot+"/"+ResponsePath)
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
            generateAuthRequest(r,we.generateUrlWithUsername(""),"%s/%s".format(PathRoot,ResponsePath))
					}
				}
      })
    }
    case r:Req if (!attemptInProgress.is && !isLoggedIn.is && !alreadyLoggedIn) => () => {
      val hostAndPort = generateHostAndPort(r)
      val path = r.path.wholePath.mkString("/")
      val newParams = r.params.toList.sortBy(_._1).foldLeft("")((acc, param) => param match {
        case Tuple2(paramName,listOfParams) if (paramName.toLowerCase == "ticket") => acc
        case Tuple2(paramName,listOfParams) => {
          val newItem = listOfParams.map(paramValue => "%s=%s".format(urlEncode(paramName),urlEncode(paramValue))).mkString("&")
          acc match {
            case "" => acc+"?"+newItem
            case _ => acc+"&"+newItem
          }
        }
        case _ => acc
      })
      val finalLocation = newParams.length match {
        case 0 => "%s/%s".format(hostAndPort,path)
        case _ => "%s/%s%s".format(hostAndPort,path,newParams)
      }

      OriginalRequestPath(Full(finalLocation))
      getAllEndpoints match {
        case Nil => {
          Full(UnauthorizedResponse(finalLocation))
        }
        case List(onlyOne) => {
          Full(RedirectResponse("/openIdGoTo/"+onlyOne.name))  
        }
        case moreThanOne:List[OpenIdEndpoint] => {
          Full(XhtmlResponse(
            <html xmlns="http://www.w3.org/1999/xhtml">
              <body>
                <div>
                {
                  moreThanOne.map(we => {
                    a(<span><div><img alt={we.name} src={we.imageSrc}/></div><div>{we.name}</div></span>,RedirectTo("/openIdGoTo/"+we.name))
                  })
                }</div>
              </body>
            </html>
          ,Empty,Nil,Nil,200,true))
        }
      }
    }
  }
	override def logUserOut:Unit = {}
	override def displayUser(id:OpenIdAuthenticator.this.UserType):NodeSeq = NodeSeq.Empty
	override def currentUser:Box[OpenIdAuthenticator.this.UserType] = Empty
	override def postLogin(id:Box[Identifier], res:VerificationResult):Unit = {}
}
