package com.metl.utils

import java.io._
import org.apache.http._
import org.apache.http.params._
import org.apache.http.util._
import org.apache.http.client.methods._
import org.apache.http.auth._
import org.apache.http.message._
import org.apache.http.conn._
import org.apache.http.conn.ssl._
import org.apache.http.conn.scheme._
import org.apache.http.impl.client._
import org.apache.http.impl.conn._
import org.apache.http.impl.conn.tsccm._
import java.util.concurrent._
import java.util.concurrent.TimeUnit._
import javax.net.ssl._
import java.security.cert._
import scala.actors.Futures._
import java.util.ArrayList
import org.apache.http.protocol.HTTP
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.entity.{StringEntity,ByteArrayEntity}
import org.apache.http.message.BasicNameValuePair
import org.apache.commons.io.IOUtils
import org.apache.http.message.{BasicHttpRequest,BasicHttpEntityEnclosingRequest}
import org.apache.http.conn.routing.HttpRoute
import java.net.URI
import org.apache.http.params.BasicHttpParams
import org.apache.http.protocol.BasicHttpContext
import net.liftweb.util._

case class RetryException(message:String,exceptions:List[Throwable] = List.empty[Throwable]) extends Exception(message){}
case class RedirectException(message:String,exceptions:List[Throwable] = List.empty[Throwable]) extends Exception(message){}
case class WebException(message:String,code:Int,path:String) extends Exception(message){}

trait IMeTLHttpClient {
  def addAuthorization(domain:String,username:String,password:String):Unit

  def get(uri:String):String = get(uri,List.empty[(String,String)])
  def get(uri:String,additionalHeaders:List[(String,String)]): String

  def getAsString(uri:String):String = getAsString(uri,List.empty[(String,String)])
  def getAsString(uri:String,additionalHeaders:List[(String,String)]):String

  def getAsBytes(uri:String):Array[Byte] = getAsBytes(uri, List.empty[(String,String)])
  def getAsBytes(uri:String,additionalHeaders:List[(String,String)]):Array[Byte]

  def postBytes(uri:String,bytes:Array[Byte]): Array[Byte] = postBytes(uri, bytes, List.empty[(String,String)])
  def postBytes(uri:String,bytes:Array[Byte],additionalHeaders:List[(String,String)]):Array[Byte]

  def postForm(uri:String,postItemList:List[(String,String)]):Array[Byte] = postForm(uri, postItemList, List.empty[(String,String)])
  def postForm(uri:String,postItemList:List[(String,String)],additionalHeaders:List[(String,String)]):Array[Byte]

  def postUnencodedForm(uri:String,postItemList:List[(String,String)]):Array[Byte] = postUnencodedForm(uri, postItemList, List.empty[(String,String)])
  def postUnencodedForm(uri:String,postItemList:List[(String,String)],additionalHeaders:List[(String,String)]):Array[Byte]

  def setCookies(cookies: Map[String,Header]): Unit
  def getCookies: Map[String,Header]

  def setHttpHeaders(headers:List[Header]): Unit
  def getHttpHeaders: List[Header]
}

class CleanHttpClient(connMgr:ClientConnectionManager) extends DefaultHttpClient(connMgr) with IMeTLHttpClient {
  protected val connectionTimeout = 120
  //protected val connectionTimeout = 30
  protected val keepAliveTimeout = 120
  protected val readTimeout = 240000
  protected val httpParams = getParams
  protected def httpContext = createHttpContext
  protected val maxRedirects = 20
  //protected val maxRetries = 30
  protected val maxRetries = 2
  private var authorizations:Map[String,(String,String)] = Map.empty[String,(String,String)].withDefault((location) => ("anonymous","unauthorized"))
  protected var cookies = Map.empty[String,Header]
  protected var httpHeaders = {
    Array[Header](
      new BasicHeader("Connection","keep-alive")
    )
  }

  override def setCookies(cook:Map[String,Header]):Unit = cookies = cook
  override def getCookies : Map[String, Header] = cookies

	def addHttpHeader(name:String,value:String):Unit = setHttpHeaders(getHttpHeaders ::: List(new BasicHeader(name,value)))
  override def setHttpHeaders(headers:List[Header]):Unit = httpHeaders = headers.toArray
  override def getHttpHeaders : List[Header] = httpHeaders.toList

  private def addAdditionalHeaders(message:AbstractHttpMessage,additionalHeaders:List[(String,String)]):Unit = {
    if (additionalHeaders.length > 0)
      additionalHeaders.foreach(header => message.addHeader(new BasicHeader(header._1,header._2)))
  }
  override def addAuthorization(domain:String,username:String,password:String):Unit = {
    authorizations = authorizations.updated(domain,(username,password))
  }
  private def applyDefaultHeaders(message:AbstractHttpMessage, uri:URI):Unit = {
    httpHeaders.foreach(message.addHeader)
    applyHostHeader(message,uri)
    applyClientCookies(message,uri)
    applyAuthHeader(message,uri)
  }
  private def applyAuthHeader(message:AbstractHttpMessage,uri:URI):Unit = {
    val host = uri.toString
    val constructedHost = determineScheme(uri)+"://"+determineHost(uri)+":"+determinePort(uri)+"/"+determinePath(uri)
    authorizations.map(auth => auth match {
      case (domain,("anonymous","unauthorized")) => {}
      case (domain,(username,password)) if (host.contains(domain) || constructedHost.contains(domain) || domain == "" || domain == "*") => {
        val authorizationString = SecurityHelpers.base64Encode(("%s:%s".format(username,password)).getBytes("UTF-8"))
        message.addHeader(new BasicHeader("Authorization","Basic %s".format(authorizationString)))
      }
    })
  }
  private def applyHostHeader(message:AbstractHttpMessage,uri:URI):Unit = {
    val port = determinePort(uri)
    val host = determineHost(uri)
    val blacklist = List(80,443)
    if (blacklist.contains(port))
      message.addHeader(new BasicHeader("Host",host))
    else
      message.addHeader(new BasicHeader("Host","%s:%s".format(host,port)))
  }
  private def withConn(uri:String,actOnConn:(ManagedClientConnection,String) => Unit,redirectNumber:Int = 0,retryNumber:Int = 0, exceptionsSoFar:List[Throwable] = List.empty[Throwable]):Array[Byte] = Stopwatch.time("Http.withConn", () => {
    try {
      if (maxRedirects > 0 && redirectNumber > maxRedirects) throw new RedirectException("exceeded configured maximum number of redirects (%s) when requesting: %s".format(maxRedirects,uri),exceptionsSoFar)
      if (maxRetries > 0 && retryNumber > maxRetries) throw new RetryException("exceeded configured maximum number of retries (%s) when requesting: %s".format(maxRetries,uri),exceptionsSoFar)
      val correctlyFormedUrl = new URI(uri)
      val port = determinePort(correctlyFormedUrl)
      val host = determineHost(correctlyFormedUrl)
      val path = determinePath(correctlyFormedUrl)
      val scheme = determineScheme(correctlyFormedUrl)
      val query = determineQuery(correctlyFormedUrl)
			println("REQ: S:%s H:%s Port:%s Path: %s Query:%s".format(scheme,host,port,path,query)) 
      val route = new HttpRoute(new HttpHost(host,port,scheme))
      val connRequest = connMgr.requestConnection(route,null)
      val conn = connRequest.getConnection(connectionTimeout,TimeUnit.SECONDS)
      try {
        conn.open(route,httpContext,httpParams)
        val relPath = path+query
        actOnConn(conn,relPath)
        if(conn.isResponseAvailable(readTimeout)){
          val response = conn.receiveResponseHeader
          storeClientCookies(response.getHeaders("Set-Cookie").toList)
//					println("response: %s".format(response.getStatusLine.getStatusCode))
          val output = response.getStatusLine.getStatusCode match {
            case 200 => {
              conn.receiveResponseEntity(response)
              val entity = response.getEntity
              val tempOutput = IOUtils.toByteArray(entity.getContent)
              EntityUtils.consume(entity)
              tempOutput
            }
            case 300 | 301 | 302 | 303 => {
              val newLoc = response.getHeaders("Location").first.getValue
              val oldLocUri = new URI(uri)
              val newLocUri = new URI(newLoc)
              val newLocString = if (newLocUri.getHost == null) oldLocUri.resolve(newLocUri).toString else newLoc
              conn.abortConnection
              withConn(newLocString,actOnConn,redirectNumber + 1, retryNumber, exceptionsSoFar ::: List(new RedirectException("healthy redirect from: %s to %s".format(oldLocUri,newLocUri))))
            }
            case 400 => throw new WebException("bad request sent to %s".format(uri),400,uri)
            case 401 => throw new WebException("access to object at %s requires authentication".format(uri),401,uri)
            case 403 => throw new WebException("access forbidden to object at %s".format(uri),403,uri)
            case 404 => throw new WebException("object not found at %s".format(uri),404,uri)
            case 500 => throw new Exception("server error encountered at %s".format(uri))
/*          case 500 => {
              conn.receiveResponseEntity(response)
              val entity = response.getEntity
              val tempOutput = IOUtils.toByteArray(entity.getContent)
              EntityUtils.consume(entity)
              tempOutput
						}
*/
            case other => throw new Exception("http status code (%s) not yet implemented, returned from %s".format(other,uri))
          }
          connMgr.releaseConnection(conn,keepAliveTimeout,TimeUnit.SECONDS)
          output
        }
        else {
          connMgr.releaseConnection(conn,keepAliveTimeout,TimeUnit.SECONDS)
          throw new Exception("Socket timeout - No data from: %s".format(uri))
        }
      } catch {
        case ex:RetryException => {
          throw ex
        }
        case ex:RedirectException => {
          throw ex
        }
				case ex:WebException => {
					throw ex
				}
        case ex:Throwable => {
          conn.abortConnection
          withConn(uri,actOnConn,redirectNumber,retryNumber + 1, exceptionsSoFar ::: List(ex))
        }
      }
    } catch {
      case ex:Throwable => {
        throw ex
      }
    }
  })
  private def displayMetrics(conn:ManagedClientConnection):Unit = {
    val m = conn.getMetrics
    println("sent: %s (%s bytes)".format(m.getRequestCount,m.getSentBytesCount))

    println("rec'd:     %s (%s bytes)".format(m.getResponseCount,m.getReceivedBytesCount))
  }
  override def postBytes(uri:String,bytes:Array[Byte],additionalHeaders:List[(String,String)] = List.empty[(String,String)]):Array[Byte] = Stopwatch.time("Http.post[Array[Byte]]", () => {
    val correctlyFormedUrl = new URI(uri)
    val bytePostingPost = (conn:ManagedClientConnection,path:String) => {
      val postMethod = new BasicHttpEntityEnclosingRequest("POST",path){override val expectContinue = false}
      val postEntity = new ByteArrayEntity(bytes)
      applyDefaultHeaders(postMethod,correctlyFormedUrl)
      addAdditionalHeaders(postMethod,additionalHeaders)
      postMethod.setEntity(postEntity)
      postMethod.addHeader(new BasicHeader("Content-Length",postEntity.getContentLength.toString))
      conn.sendRequestHeader(postMethod)
      conn.sendRequestEntity(postMethod)
      conn.flush
    }
    withConn(uri,bytePostingPost)
  })
  override def postForm(uri:String,postItemList:List[(String,String)],additionalHeaders:List[(String,String)] = List.empty[(String,String)]):Array[Byte] = Stopwatch.time("Http.post[List[(String,String)]]", () => {
    val correctlyFormedUrl = new URI(uri)
    val formPostingPost = (conn:ManagedClientConnection,path:String) => {
      val postMethod = new BasicHttpEntityEnclosingRequest("POST",path){override val expectContinue = false}
      val postForm = new ArrayList[NameValuePair]
      for (postItem <- postItemList){
        postForm.add(new BasicNameValuePair(postItem._1,postItem._2))
      }
      val postEntity = new UrlEncodedFormEntity(postForm,HTTP.UTF_8)
      applyDefaultHeaders(postMethod,correctlyFormedUrl)
      addAdditionalHeaders(postMethod,additionalHeaders)
      postMethod.addHeader(new BasicHeader("Content-Type","""application/x-www-form-urlencoded"""))
      postMethod.addHeader(new BasicHeader("Content-Length",postEntity.getContentLength.toString))
      postMethod.setEntity(postEntity)
      conn.sendRequestHeader(postMethod)
      conn.sendRequestEntity(postMethod)
      conn.flush
    }
    withConn(uri,formPostingPost)
  })
  override def postUnencodedForm(uri:String,postItemList:List[(String,String)],additionalHeaders:List[(String,String)] = List.empty[(String,String)]):Array[Byte] = Stopwatch.time("Http.postUnencodedForm", () => {
    val correctlyFormedUrl = new URI(uri)
    val unencodedFormPostingPost = (conn:ManagedClientConnection,path:String) => {
      val postMethod = new BasicHttpEntityEnclosingRequest("POST",path){override val expectContinue = false}
      val postForm = postItemList.map(postItem => postItem._1 +"="+postItem._2).mkString("&")
      val postEntity = new StringEntity(postForm,HTTP.UTF_8)
      applyDefaultHeaders(postMethod,correctlyFormedUrl)
      addAdditionalHeaders(postMethod,additionalHeaders)
      postMethod.addHeader(new BasicHeader("Content-Type","""application/x-www-form-urlencoded"""))
      postMethod.addHeader(new BasicHeader("Content-Length",postEntity.getContentLength.toString))
      postMethod.setEntity(postEntity)
      conn.sendRequestHeader(postMethod)
      conn.sendRequestEntity(postMethod)
      conn.flush
    }
    withConn(uri,unencodedFormPostingPost)
  })
  override def get(uri:String,additionalHeaders:List[(String,String)] = List.empty[(String,String)]):String = Stopwatch.time("Http.get", () => {
    getAsString(uri,additionalHeaders)
  })
  override def getAsString(uri:String,additionalHeaders:List[(String,String)] = List.empty[(String,String)]):String = Stopwatch.time("Http.getAsString", () => {
    IOUtils.toString(getAsBytes(uri,additionalHeaders))
  })
  override def getAsBytes(uri:String,additionalHeaders:List[(String,String)] = List.empty[(String,String)]):Array[Byte] = Stopwatch.time("Http.getAsBytes", () => {
    val correctlyFormedUrl = new URI(uri)
    val bytesGettingGet = (conn:ManagedClientConnection,path:String) => {
      val getMethod = new BasicHttpRequest("GET",path)
      applyDefaultHeaders(getMethod,correctlyFormedUrl)
      addAdditionalHeaders(getMethod,additionalHeaders)
      conn.sendRequestHeader(getMethod)
      conn.flush
    }
    withConn(uri,bytesGettingGet)
  })

  private def determinePort(uri:URI):Int = {
    uri.getPort match {
      case other:Int if (other == -1) => uri.getScheme match {
        case "http" => 80
        case "https" => 443
        case other:String => 80
        case _ => 80
      }
      case other:Int => other
      case _ => 80
    }
  }
  private def determineHost(uri:URI):String = {
    uri.getHost match {
      case null => throw new IllegalArgumentException("No hostname supplied")
      case host:String => host
    }
  }
  private def determinePath(uri:URI):String = {
    val finalPath = uri.getPath match {
      case "" => "/"
      case path:String => path
      case _ => "/"
    }
    "/"+finalPath.dropWhile(c => c == '/')
  }
  private def determineScheme(uri:URI):String = {
    uri.getScheme match {
      case null => "http"
      case other:String => other
    }
  }
  private def determineConnSecurity(uri:URI):Boolean = {
    uri.getScheme match {
      case "https" => true
      case _ => false
    }
  }
  private def determineQuery(uri:URI):String = {
    val finalQuery = uri.getRawQuery match {
      case "" => ""
      case null => ""
      case other:String => "?"+other
    }
    finalQuery.dropWhile(c => c == '/')
  }
  private def applyClientCookies(request:AbstractHttpMessage,uri:URI) = {
    val cookieString = cookies.map(cookieList => {
      val cookiesContained = cookieList._2.getElements.toList
      cookiesContained.map(cookie =>{
        val cookieName = cookie.getName
        val cookieValue = cookie.getValue
        val cookieParams = cookie.getParameters
        val cookieDomain = cookieParams.find(p => p.getName.toLowerCase == "domain") match {
          case Some(dom) => dom.getValue
          case None => "/"
        }
        val testTrue = uri.toString.toLowerCase.contains(cookieDomain.toLowerCase)
        if (testTrue)
          "%s=%s".format(cookieName,cookieValue)
        else ""
      })
    }).flatten.mkString("; ").trim.dropWhile(c => c == ';').reverse.dropWhile(c => c == ';').reverse.trim
    request.addHeader(new BasicHeader("Cookie",cookieString))
  }
  private def storeClientCookies(newHeaders:List[Header]):Unit = {
    val newCookies = newHeaders.map(header => header.getElements.toList.map(item => (item.getName, header)))
    newCookies.foreach(newCookieList => newCookieList.foreach(newCookie => cookies = cookies.updated(newCookie._1,newCookie._2)))
  }
}

object Http{
  private object TrustAllHosts extends HostnameVerifier{
    override def verify(_host:String,_session:SSLSession)= true
  }
  private object TrustingTrustManager extends X509TrustManager{
    override def getAcceptedIssuers = Array.empty[X509Certificate]
    override def checkClientTrusted(certs:Array[X509Certificate], t:String) = ()
    override def checkServerTrusted(certs:Array[X509Certificate], t:String) = ()
  }
  private val getSchemeRegistry = {
    val ssl_ctx = SSLContext.getInstance("TLS")
    val managers = Array[TrustManager](TrustingTrustManager)
    ssl_ctx.init(null, managers, new java.security.SecureRandom())
    val sslSf = new org.apache.http.conn.ssl.SSLSocketFactory(ssl_ctx, org.apache.http.conn.ssl.SSLSocketFactory.ALLOW_ALL_HOSTNAME_VERIFIER)
    val schemeRegistry = new SchemeRegistry()
    schemeRegistry.register(new Scheme("https", 443, sslSf))
    schemeRegistry.register(new Scheme("http", PlainSocketFactory.getSocketFactory,80))
    schemeRegistry
  }
  private val getConnectionManager = {
    val connMgr = new ThreadSafeClientConnManager(getSchemeRegistry)
    connMgr.setDefaultMaxPerRoute(500)
    connMgr.setMaxTotal(1000)
    connMgr
  }
  def getClient = Stopwatch.time("Http.getClient", () => {
    new CleanHttpClient(getConnectionManager)
  })
  def getAuthedClient(username:String,password:String,domain:String = "*") = Stopwatch.time("Http.getAuthedClient", () => {
    val client = new CleanHttpClient(getConnectionManager)
    client.addAuthorization(domain,username,password)
    client
  })
  def getCASEDClient(uri:String,username:String,password:String):CleanHttpClient = Stopwatch.time("Http.getCASEDClient", () => {
    val client = getClient
    val postUrl = """https://my.monash.edu.au/login"""
    val postItemList = List(("access","authcate"),("username",username),("password",password),("request_uri","""/authentication/cas/login?service=%s""".format(uri)),("submit","login")).asInstanceOf[List[(String,String)]]
    client.postForm(postUrl,postItemList)
    client
  })
  def cloneClient(incoming:CleanHttpClient):CleanHttpClient = Stopwatch.time("Http.cloneClient", () => {
    val client = new CleanHttpClient(getConnectionManager)
    client.setCookies(incoming.getCookies)
    client.setHttpHeaders(incoming.getHttpHeaders)
    client
  })
  def getClient(headers:List[(String,String)]):CleanHttpClient = Stopwatch.time("Http.getClient(headers)", () => {
    val newHeaders = headers.map(tup => new BasicHeader(tup._1,tup._2)).toList
    val client = new CleanHttpClient(getConnectionManager){
      override val connectionTimeout = 3600
      override val keepAliveTimeout = 5400
      override val readTimeout = 7200000
    }
    client.setHttpHeaders(newHeaders)
    client
  })
}
