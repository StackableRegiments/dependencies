package com.metl.utils

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import net.liftweb.common.{Empty, Full}
import net.liftweb.http.{InMemoryResponse, S}
import org.apache.commons.codec.digest.DigestUtils
import net.liftweb.util.Helpers._

case class CachedBinary(data:Array[Byte],createTime:Long) {
  lazy val checksum = DigestUtils.shaHex(data)
}

class HttpCacher {
  private def requestEtag = S.request match {
    case Full(req) => req.header("If-None-Match")
    case _ => Empty
  }

  private val formatter = new SimpleDateFormat("EEE', 'dd' 'MMM' 'yyyy' 'HH:mm:ss' 'Z", Locale.US);
  private def makeCacheHeaders(binary:CachedBinary,expiry:Long) = {
    if(expiry == 0) Nil
    else {
      val maxAge = (expiry-(new Date().getTime-binary.createTime))/1000
      List(
        "Expires"       -> formatter.format(new Date(binary.createTime+expiry)),
        "Cache-Control" -> "max-age=%d, must-revalidate".format(if (maxAge < 0) 0 else maxAge),
        "ETag"          -> binary.checksum)
    }
  }

  def constructResponse(binary:CachedBinary, contentType:String, expiry:TimeSpan) = {
    requestEtag match {
      case Full(etag) if (etag == binary.checksum) => InMemoryResponse(Array.empty[Byte], List(), Nil, 304)
      case _ => InMemoryResponse(binary.data, List("Content-Type" -> contentType) ::: makeCacheHeaders(binary,expiry),Nil,200)
    }
  }
}
