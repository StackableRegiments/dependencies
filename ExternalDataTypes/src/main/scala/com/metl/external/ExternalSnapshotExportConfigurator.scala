package com.metl.external

import scala.xml.NodeSeq

abstract class ExternalSnapshotExportConfigurator {
  // the conversationRenderer takes a conversationJid, and returns a 2 tuple, of conversationTitle and a Map of slideJids associated with their JPEGs as serialized in byte arrays.
  def configureFromXml(in:NodeSeq,conversationRenderer:String => Tuple2[String,Map[String,Array[Byte]]]):Either[Exception,List[Object]]
}
