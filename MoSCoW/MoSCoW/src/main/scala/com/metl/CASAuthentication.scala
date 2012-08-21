package com.metl.model
import net.liftweb.http._
import net.liftweb.common._
import scala.collection.immutable.List
import net.liftweb.http.provider.HTTPCookie
import java.net.URLEncoder
import org.apache.commons.io.IOUtils

case class CASStateData(authenticated:Boolean,username:String,eligibleGroups:Seq[(String,String)],informationGroups:Seq[(String,String)])
object CASStateData {
	val forbidden = CASStateData(false,"forbidden",List.empty[Tuple2[String,String]],List.empty[Tuple2[String,String]])
}

object casStateDevelopmentData {
  lazy val all = List(
    CASStateData(false,"Roger",List(("ou","Unrestricted"),("uid","UnauthenticatedRoger"),("ou","Student"),("enrolledsubject","PSY2011"),("enrolledsubject","ENG2011"),("enrolledsubject","PHS2012"),("enrolledsubject","BIO2011")),List(("givenname","Roger"),("sn","Dodger"),("mail","roger.dodger@monash.edu"),("cn","Rogey"),("initials","RD"),("gender","male"),("personaltitle","mr"))),
    CASStateData(false,"Jane",List(("ou","Unrestricted"),("uid","UnauthenticatedJane"),("ou","Student"),("enrolledsubject","PSY2011"),("enrolledsubject","ENG2011"),("enrolledsubject","PHS2012"),("enrolledsubject","BIO2011")),List(("givenname","Jane"),("sn","Normal"),("mail","jane.normal@monash.edu"),("cn","Janey"),("initials","JN"),("gender","female"),("personaltitle","mrs"))),
    CASStateData(false,"John",List(("ou","Unrestricted"),("uid","UnauthenticatedJohn"),("ou","Student"),("enrolledsubject","PSY2011"),("enrolledsubject","ENG2011"),("enrolledsubject","PHS2012"),("enrolledsubject","BIO2011")),List(("givenname","John"),("sn","Doe"),("mail","John.Doe@monash.edu"),("cn","Jonno"),("initials","JD"),("gender","male"),("personaltitle","mr"))), 
    CASStateData(false,"Dick",List(("ou","Unrestricted"),("uid","UnauthenticatedDick"),("ou","Staff"),("monashteachingcommitment","PSY2011"),("monashteachingcommitment","ENG2011"),("monashteachingcommitment","PHS2012"),("monashteachingcommitment","BIO2011")),List(("givenname","Dick"),("sn","Tracey"),("mail","richard.tracey@monash.edu"),("cn","Dickey"),("initials","DT"),("gender","male"),("personaltitle","dr")))
  )
  def state(user:String)=all.filter(_.username == user).toList match{
    case List(data, _) => data
    case _ => CASStateData(false,user,List(("ou","Unrestricted"),("uid","Unauthenticated"+user),("ou","Student")),List(("givenname",user),("sn","Tracey"),("mail",user+"@monash.edu"),("cn",user+"y"),("initials",user.take(2)),("gender","male"),("personaltitle","dr")))
  }
  val default = all(0)
}


object CASAuthenticator {
	def attachCASAuthenticator(mod:CASAuthenticator):Unit = {
		LiftRules.dispatch.prepend {
			case req if !mod.casState.authenticated && !mod.checkReqForCASCookies(req) => () => Full(mod.CASRedirect) 
		}
	}	
}

class CASAuthenticator(realm:String,onSuccess:(CASStateData) => Unit) {

	object casState extends SessionVar[CASStateData](CASStateData.forbidden)

	def getCasState = casState.is

  val monashCasUrl = "https://my.monash.edu.au/authentication/cas"
  def checkReqForCASCookies(req:Req):Boolean = {
		val result = verifyCASTicket(req)
		if (result.authenticated){
			onSuccess(result)
			true
		} else {
			false
		}
	}
  private def ticketlessUrl(originalRequest : Req):String = {
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
     case 0 => "%s://%s:%s/".format(originalRequest.request.scheme, url,port)
     case _ => "%s://%s:%s/%s%s".format(originalRequest.request.scheme, url,port, path, newParams)
   }
  }
  private def verifyCASTicket(req:Req) : CASStateData = {
      req.param("ticket") match {
      case Full(ticket) =>
      {
        val verifyUrl = monashCasUrl + "/serviceValidate?ticket=%s&service=%s"
	      val casValidityResponse = Http.getClient.getAsString(verifyUrl.format(ticket,URLEncoder.encode(ticketlessUrl(req), "utf-8")))
        val casValidityResponseXml = xml.XML.loadString(casValidityResponse)
	      val state = for(success <- (casValidityResponseXml \\ "authenticationSuccess");
					user <- (success \\ "user");
					groups <- LDAP.ou(List(user.text)).get(user.text);
					info <- LDAP.info(List(user.text)).get(user.text)
				) yield CASStateData(true,user.text,groups,info)
				state match{
					case List(state@CASStateData(true,fetchedUsername,_,_))=> state
					case _ => CASStateData.forbidden
				}
      }
      case Empty => CASStateData.forbidden
      case _ => CASStateData.forbidden
    }
  }
  val redirectUrl = monashCasUrl + "/login/?service=%s" 
  def CASRedirect = {
		val req = S.request.openTheBox
		val url = redirectUrl.format(URLEncoder.encode(ticketlessUrl(req),"utf-8"))
		new RedirectResponse(url, req)
  }
}
