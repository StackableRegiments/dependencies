package com.metl.orientDB

import javax.persistence.{Version, Id}

import com.metl.data._
import com.metl.utils._

import net.liftweb.common._
import net.liftweb.util.Helpers._

import com.orientechnologies.orient.server.OServerMain

class OData {
	@Id var id: String = _
	var typeDescriptor: String = _
	@Version var version: java.lang.Integer = _
	override def toString = "%s(Id(%s),Version(%s))".format(typeDescriptor,id,version)
}

class OResource extends OData {
	typeDescriptor = "Resource"
	var jid: String = _
	var identifier: String = _
	var bytes: Array[Byte] = _
	override def toString = "%s(Id(%s),Jid(%s),Identifier(%s),Version(%s))".format(typeDescriptor,id,jid,identifier,version)
}
