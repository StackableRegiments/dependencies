package com.metl.h2

import com.metl.utils._
import com.metl.data._
import com.metl.persisted._

class LocalH2BackendAdaptor(name:String) extends PersistedAdaptor(name,"localhost"){
	override lazy val dbInterface = new H2Interface(name)
	override def shutdown = dbInterface.shutdown
}
