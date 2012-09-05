package com.metl.model

import com.metl.utils._
import com.metl.data._
import com.metl.liftExtensions._

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import java.util.{Date,Locale}
import java.text.SimpleDateFormat

object HttpResponder extends HttpCacher{

	private val snapshotExpiry = 10 seconds
	private val quizImageExpiry = 30 seconds

	def snapshot(server:String,jid:String,size:String) ={
		val serverConfig = ServerConfiguration.configForName(server)
		val binary = HistoryCache.getSnapshot(jid,serverConfig,SnapshotSize.parse(size))
		constructResponse(binary,"image/jpg",snapshotExpiry)
	}

	def quizImage(server:String,jid:String,id:String) ={
		val serverConfig = ServerConfiguration.configForName(server)
		val binary = HistoryCache.getQuizImage(jid,serverConfig,id)
		constructResponse(binary,"image/jpg",quizImageExpiry)
	}
}
