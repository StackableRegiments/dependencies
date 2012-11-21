package com.metl.orientDB

import com.metl.utils._
import com.metl.data._
import com.metl.persisted._

class LocalOrientDBBackendAdaptor(name:String) extends PersistedAdaptor(name,"localhost"){
	override val dbInterface = new OrientDBInterface(name)
}
