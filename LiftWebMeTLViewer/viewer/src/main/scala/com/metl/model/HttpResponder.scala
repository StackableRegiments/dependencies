package com.metl.model

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import java.util.{Date,Locale}
import java.text.SimpleDateFormat

object HttpResponder {

	private val snapshotExpiry = 10 seconds
	private val quizImageExpiry = 30 seconds

	private def requestEtag = S.request match {
		case Full(req) => req.header("If-None-Match")
		case _ => Empty
	}

	private val formatter = new SimpleDateFormat("EEE', 'dd' 'MMM' 'yyyy' 'HH:mm:ss' 'Z", Locale.US);
	def makeCacheHeaders(binary:CachedBinary,expiry:Long) = List(
		"Expires"       -> formatter.format(new Date(binary.createTime+expiry)),
		"Cache-Control" -> "max-age=%d, must-revalidate".format((expiry-(new Date().getTime-binary.createTime))/1000),
		"ETag"          -> binary.checksum
	)

	def snapshot(server:String,jid:String,size:String) ={
		val serverConfig = ServerConfiguration.configForName(server)
		val binary = HistoryCache.getSnapshot(jid,serverConfig,SnapshotSize.parse(size))
		requestEtag match {
			case Full(etag) if (etag == binary.checksum) => InMemoryResponse(Array.empty[Byte], List(), Nil, 304)
			case _ => InMemoryResponse(binary.data, List("Content-Type" -> "image/jpg") ::: makeCacheHeaders(binary,snapshotExpiry), Nil, 200)
		}
	}

	def quizImage(server:String,jid:String,id:String) ={
		val serverConfig = ServerConfiguration.configForName(server)
		val binary = HistoryCache.getQuizImage(jid,serverConfig,id)
		requestEtag match {
			case Full(etag) if (etag == binary.checksum) => InMemoryResponse(Array.empty[Byte], List(), Nil, 304)
			case _ => InMemoryResponse(binary.data, List("Content-Type" -> "image/jpg") ::: makeCacheHeaders(binary,quizImageExpiry), Nil, 200)
		}
	}
}
