package com.metl.embedded

import com.metl.data._
import com.metl.utils._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import java.util.Date

class EmbeddedHistory(serverName:String,dbInterface:LocalEmbeddedInterface) extends HistoryRetriever(serverName) {
  def getMeTLHistory(jid:String) = Stopwatch.time("EmbeddedHistory.getMeTLHistory", () => {
    History.empty
  })
}
