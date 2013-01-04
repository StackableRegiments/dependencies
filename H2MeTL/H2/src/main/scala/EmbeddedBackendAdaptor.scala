package com.metl.h2

import com.metl.utils._
import com.metl.data._
import com.metl.persisted._
import scala.xml._

object LocalH2ServerConfiguration{
	def initialize = ServerConfiguration.addServerConfigurator(LocalH2ServerConfigurator) 
}

class LocalH2BackendAdaptor(name:String) extends PersistedAdaptor(name,"localhost"){
	override lazy val dbInterface = new H2Interface(name)
	override def shutdown = dbInterface.shutdown
}
object LocalH2ServerConfigurator extends ServerConfigurator{
	override def matchFunction(e:Node) = (e \\ "type").text == "localH2"
	override def interpret(e:Node) = Some(new LocalH2BackendAdaptor("localH2"))
}
