package com.metl.metl2011

import com.metl.data._
import com.metl.utils._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import java.util.zip.{ZipInputStream,ZipEntry}
import org.apache.commons.io.IOUtils
import scala.xml.NodeSeq
import java.io.ByteArrayInputStream
import java.util.Date

class MeTL2011History(serverName:String,http:HttpProvider) extends HistoryRetriever(serverName) {
  val utils = new MeTL2011Utils(serverName)
  private def publicHistoryUrl(jid:String) = "https://%s:1749/%s/%s/all.zip".format(server.host,utils.stem(jid.toString),jid)
  private def privateHistoryUrl(username:String,jid:String) = "https://%s:1749/%s/%s/%s/all.zip".format(server.host,utils.stem(username.toString),username,jid)
  def getMeTLHistory(jid:String):History = Stopwatch.time("MeTL2011History.getMeTLHistory", () => {
    val url = jid.dropWhile(_.isDigit) match {
      case username:String if (username.length > 0) => privateHistoryUrl(username,jid.takeWhile(_.isDigit))
      case _ => publicHistoryUrl(jid)
    }
    println("fetching history from url: %s".format(url))
    val downloadedBytes = Stopwatch.time("MeTL2011History.getMeTLHistory.fetch", () => http.getClient.getAsBytes(url))
    val stream = new ByteArrayInputStream(downloadedBytes)
    val zipStream = new java.util.zip.ZipInputStream(stream)
    def parseMessages(inputStream:ZipInputStream):List[MeTLStanza] = {
      try {
        inputStream.getNextEntry match {
          case ze:ZipEntry if ze.getName.endsWith(".xml") => tryo(dailyXmlToListOfStanzas(xml.XML.loadString(IOUtils.toString(inputStream)+"</logCollection>"))).openOr(List.empty[MeTLStanza]) ::: parseMessages(inputStream)
          case ze:ZipEntry => parseMessages(inputStream)
          case _ => List.empty[MeTLStanza]
        }
      } catch {
        case e:Throwable => {
          List.empty[MeTLStanza]
        }
      }
    }
    val messages = Stopwatch.time("MeTL2011History.getMeTLHistory.unzipAndParse", () => parseMessages(zipStream).toList)
    zipStream.close
    Stopwatch.time("MeTL2011History.getMeTLHistory.makeHistory", () => makeHistory(jid.toString,messages))
  })
  def dailyXmlToListOfStanzas(input:NodeSeq):List[MeTLStanza] = Stopwatch.time("History.dailyXmlToListOfStanzas", () => {
    val serializer = new MeTL2011XmlSerializer(serverName,true)
    (input \\ "message").map(i => serializer.toMeTLStanza(i)).toList
  })
}
