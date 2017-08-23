package com.metl.persisted

import com.metl.data._
import com.metl.utils._

class PersistedResourceProvider(config:ServerConfiguration,dbInterface:PersistenceInterface,commonBucket:String = "commonBucket") {
  def getResource(identity: String): Array[Byte] = dbInterface.getResource(identity)
  def getResource(jid: String, identity: String): Array[Byte] = dbInterface.getResource(jid, identity)
}