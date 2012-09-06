package com.metl.model

import net.liftweb.common._

object Authenticator {
	def authenticate(username:String,password:String):String = Stopwatch.time("Authenticator.authenticate",() => {
		if (LDAP.authenticate(username,password)) {
			val sessionPass = DBFormats.createObjectId.toString
			updateUserRecord(username,sessionPass)
			"your session pass is: "+sessionPass
		} else {
			"you have supplied invalid credentials"
		}
	})
	def checkSessionCookie(username:String,sessionPass:String):Boolean = Stopwatch.time("Authenticator.checkSessionCookie", () => User.fromUsername(username).checkSession(sessionPass))	
	private def updateUserRecord(username:String,sessionPass:String) = Stopwatch.time("Authenticator.updateUserRecord", () => User.fromUsername(username).updateSession(sessionPass))
	def inSession[A](username:String,password:String,func:(String)=>A):Box[A] = Stopwatch.time("Authenticator.inSession",() => {
		if (checkSessionCookie(username,password)){
			Full(func(username))
		} else {
			Empty
		}
	})

	def fetchPublicKey:String = Stopwatch.time("Authenticator.fetchPublicKey", () => {
		Globals.HandshakeCryptographer.is.getXmlPublicKey.toString
	})
	def authenticateWithPublicKey(username:String,encryptedPass:String):String = Stopwatch.time("Authenticator.authenticateWithPublicKey", () => {
		authenticate(username,Globals.HandshakeCryptographer.is.decryptCred(encryptedPass))
	})
}
