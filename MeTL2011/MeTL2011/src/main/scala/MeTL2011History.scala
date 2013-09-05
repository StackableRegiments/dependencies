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
  override def makeHistory(jid:String,stanzas:List[MeTLStanza]):History = Stopwatch.time("History.makeHistory", () => {
//		stanzas.foreach(s => println("IN: %s".format(s)))
    val output = stanzas.sortBy(s => s.timestamp).foldLeft(new History(jid))((h,item) => h.addStanza(item))
//		output.getAll.foreach(s => println("OUT: %s".format(s)))
		output
  })

  private def publicHistoryUrl(jid:String) = "https://%s:1749/%s/%s/".format(server.host,utils.stem(jid.toString),jid)
  private def privateHistoryUrl(username:String,jid:String) = "https://%s:1749/%s/%s/%s/".format(server.host,utils.stem(username.toString),username,jid)
  private val historySuffix = "all.zip"
  def getMeTLHistory(jid:String):History = Stopwatch.time("MeTL2011History.getMeTLHistory", () => {
    val url = jid.dropWhile(_.isDigit) match {
      case username:String if (username.length > 0) => privateHistoryUrl(username,jid.takeWhile(_.isDigit))
      case _ => publicHistoryUrl(jid)
    }
		try {
			http.getClient.get(url)
    	//println("fetching history from url: %s".format(url))
			Stopwatch.time("MeTL2011History.getMeTLHistory.fetch", () => http.getClient.getAsBytes(url + historySuffix)) match {
				case downloadedBytes:Array[Byte] if downloadedBytes.length > 0 => {
					val stream = new ByteArrayInputStream(downloadedBytes)
					val zipStream = new java.util.zip.ZipInputStream(stream)
					def parseMessages(inputStream:ZipInputStream):List[MeTLStanza] = {
						try {
							inputStream.getNextEntry match {
								case ze:ZipEntry if ze.getName.endsWith(".xml") => {
									//println("parsing: %s".format(ze.getName))
									tryo(dailyXmlToListOfStanzas(xml.XML.loadString(IOUtils.toString(inputStream)+"</logCollection>"))) match {
										case Full(a) => a ::: parseMessages(inputStream)
										case other => {
											//println("Box Failure: %s".format(other))
											List.empty[MeTLStanza] ::: parseMessages(inputStream)
										}
									} 
								}
								case ze:ZipEntry => {
									//println("ze -> %s".format(ze))
									parseMessages(inputStream)
								}
								case other => {
									//println("getNextEntry: other -> %s".format(other))
									List.empty[MeTLStanza]
								}
							}
						} catch {
							case e:Throwable => {
								//println("Exception: %s -> %s".format(e,e.getMessage))
								List.empty[MeTLStanza]
							}
						}
					}
					val messages = Stopwatch.time("MeTL2011History.getMeTLHistory.unzipAndParse", () => parseMessages(zipStream).toList)
					zipStream.close
					Stopwatch.time("MeTL2011History.getMeTLHistory.makeHistory", () => makeHistory(jid.toString,messages))
				}
				case _ => makeHistory(jid.toString,List.empty[MeTLStanza])
			}
		} catch {
			case e:Throwable => makeHistory(jid.toString,List.empty[MeTLStanza])
		}
  })
  def dailyXmlToListOfStanzas(input:NodeSeq):List[MeTLStanza] = Stopwatch.time("History.dailyXmlToListOfStanzas", () => {
    val serializer = new MeTL2011XmlSerializer(serverName,true)
    val output = (input \\ "message").map(i => serializer.toMeTLStanza(i)).toList
		//println("parsed %s messages from %s".format(output.length,input))
//		output.foreach(s => println("PARSE: %s".format(s)))
		output
  })
}
