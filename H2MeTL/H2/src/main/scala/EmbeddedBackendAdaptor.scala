package com.metl.h2

import com.metl.utils._
import com.metl.data._
import com.metl.persisted._

class LocalH2BackendAdaptor(name:String) extends PersistedAdaptor(name,"localhost"){
	override val serializer = new H2Serializer(name)
	override val dbInterface = new H2Interface(name,serializer)
}
