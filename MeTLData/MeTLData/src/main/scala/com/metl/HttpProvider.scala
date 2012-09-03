package com.metl.data

import com.metl.utils._

abstract class HttpProvider{
	def getClient:CleanHttpClient	
}

object EmptyHttpProvider extends HttpProvider {
	def getClient = Http.getClient
}

class SimpleAuthedHttpProvider(username:String,password:String) extends HttpProvider {
	def getClient = Http.getAuthedClient(username,password)
}
