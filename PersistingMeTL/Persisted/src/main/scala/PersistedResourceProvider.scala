package com.metl.persisted

import com.metl.data._
import com.metl.utils._

class PersistedResourceProvider(configName:String,dbInterface:PersistenceInterface){
	def getResource(identity:String):Array[Byte] = {
		dbInterface.getResource(identity)
	}
	def postResource(jid:String,userProposedId:String,data:Array[Byte]):String = {
		dbInterface.postResource(jid,userProposedId,data)
	}
}
