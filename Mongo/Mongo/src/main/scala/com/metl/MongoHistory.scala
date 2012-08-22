package com.metl.model

import net.liftweb.util.Helpers._
import net.liftweb.common._
import java.util.zip.{ZipInputStream,ZipEntry}
import org.apache.commons.io.IOUtils
import scala.xml.NodeSeq
import java.io.ByteArrayInputStream
import java.util.Date

class MongoHistory(serverName:String,mongoHost:String,mongoPort:Int,mongoDB:String) extends HistoryRetriever(serverName) {
  private val db = new LocalMongoInterface(mongoHost,mongoPort,mongoDB)
  def getMeTLHistory(jid:String) = Stopwatch.time("MongoHistory.getMeTLHistory", () => {
    History.empty
  })
}
