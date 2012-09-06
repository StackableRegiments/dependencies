package com.metl.model

import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.http.provider._
import Helpers._

object MeTLRestHelper extends RestHelper {
	val crossDomainPolicy = {
		<cross-domain-policy>
			<allow-access-from domain="*" />
		</cross-domain-policy>
	}
	serve {
		case Req("serverStatus" :: Nil,_,_) => 
			() => Stopwatch.time("MeTLRestHelper.serverStatus", () => Full(PlainTextResponse("OK", List.empty[Tuple2[String,String]], 200)))
		case Req("authenticate" :: "user" :: user :: "encryptedPass" :: password :: Nil,_,_) => 
			() => Stopwatch.time("MeTLRestHelper.authenticate", () => Full(PlainTextResponse(Authenticator.authenticate(user,password), List.empty[Tuple2[String,String]],200)))
		case Req("history" :: "user" :: user :: "encryptedPass" :: password :: "zone" :: zone :: "room" :: room :: Nil,_,_) => 
			() => Stopwatch.time("MeTLRestHelper.history", () => Authenticator.inSession(user,password, (user) => History.fetchAsXml(zone,room)).map(a => 
				Full(XmlResponse(a, 200))
			).openOr(Full(ForbiddenResponse("you are not permitted to access this history"))))
		case Req("pptx" :: Nil,_,_) => 
			() => Stopwatch.time("MeTLRestHelpers.pptx", () => Full(XmlResponse(OfficeConverter.convert("test.pptx"),200)))
		case Req("ppt" :: Nil,_,_) => 
			() => Stopwatch.time("MeTLRestHelpers.ppt", () => Full(XmlResponse(OfficeConverter.convert("test.ppt"),200)))
		case Req("jsonHistory" :: "user" :: user :: "encryptedPass" :: password :: "zone" :: zone :: "room" :: room :: Nil,_,_) =>
			() => Stopwatch.time("MeTLRestHelper.jsonHistory", () => Authenticator.inSession(user,password, (user) => History.fetchAsJson(zone,room)).map(a =>
				Full(PlainTextResponse(a, List.empty[Tuple2[String,String]],200))
			).openOr(Full(ForbiddenResponse("you are not permitted to access this history"))))
		case req@Req("resources" :: "user" :: user :: "encryptedPass" :: password :: "upload" :: filename :: Nil,_,_) => 
			() => Stopwatch.time("MeTLRestHelper.resources/upload",() => Authenticator.inSession(user,password,(user) => Resources.upload(filename,req.body)).map(a => 
				Full(PlainTextResponse(a, List.empty[Tuple2[String,String]],200))
			).openOr(Full(ForbiddenResponse("you are not permitted to upload this resource"))))
		case Req("resources" :: "user" :: user :: "encryptedPass" :: password :: "download" :: objectId :: Nil,_,_) => 
			() => Stopwatch.time("MeTLRestHelper.resources/download", () => Authenticator.inSession(user,password,(user) => Resources.download(objectId)).map(a => 
				Full(InMemoryResponse(a, List.empty[Tuple2[String,String]],List.empty[HTTPCookie],200))
			).openOr(Full(ForbiddenResponse("you are not permitted to download this resource"))))
		case Req("sfsServerStats" :: Nil,_,_) =>
			() => Stopwatch.time("MeTLRestHelper.sfsServerStats", () => Full(PlainTextResponse(SmartFoxServerManager.getAllStats,200)))
		case Req(List("crossdomain"),"xml",_) =>
			() => Stopwatch.time("MeTLRestHelper.crossDomainPolicy", () => Full(XmlResponse(crossDomainPolicy,200)))
	}
}
object MeTLStatefulRestHelper extends RestHelper {
	serve {
		case req@Req("publicKeyRequest" :: Nil,_,_) if (req.request.scheme == "https") =>
			() => Stopwatch.time("MeTLStatefulRestHelper.pub", () => Full(PlainTextResponse(Authenticator.fetchPublicKey, List.empty[Tuple2[String,String]], 200)))
		case req@Req("auth" :: user :: encryptedPass :: Nil,_,_) if (req.request.scheme == "https") =>
			() => Stopwatch.time("MeTLStatefulRestHelper.auth", () => Full(PlainTextResponse(Authenticator.authenticateWithPublicKey(user,encryptedPass), List.empty[Tuple2[String,String]], 200)))
	}
}
