package com.metl.orientDB

import com.metl.data._
import com.metl.utils._

import net.liftweb.common._
import net.liftweb.util.Helpers._

import com.orientechnologies.orient.server._

class OrientDBEmbeddable{
	private var server:Option[OServer] = Empty
	protected val xml = 
		<orient-server>
   		<network>
   			<protocols>
   				<protocol name="binary" implementation="com.orientechnologies.orient.server.network.protocol.binary.ONetworkProtocolBinary"/>
<!--   				<protocol name="http" implementation="com.orientechnologies.orient.server.network.protocol.http.ONetworkProtocolHttpDb"/> -->
   				<protocol name="http" implementation="com.orientechnologies.orient.server.network.protocol.http.ONetworkProtocolHttpDb"/> 
   			</protocols>
   			<listeners>
   				<listener ip-address="0.0.0.0" port-range="2424-2430" protocol="binary"/>
<!--					<listener ip-address="0.0.0.0" port-range="2480-2490" protocol="http"/> -->
					<listener ip-address="0.0.0.0" port-range="2480-2490" protocol="http"/> 
   			</listeners>
   		</network>
   		<users>
   			<user name="root" password="ThisIsA_TEST" resources="*"/>
   		</users>
   		<properties>
   			<entry name="orientdb.www.path" value="C:/work/dev/orientechnologies/orientdb/releases/1.0rc1-SNAPSHOT/www/"/>
   			<entry name="orientdb.config.file" value="C:/work/dev/orientechnologies/orientdb/releases/1.0rc1-SNAPSHOT/config/orientdb-server-config.xml"/>
   			<entry name="server.cache.staticResources" value="false"/>
   			<entry name="log.console.level" value="info"/>
				<entry name="log.file.level" value="fine"/>
   		</properties>
		</orient-server>
	def create = Stopwatch.time("OrientDBEmbeddable.create", () => {
		val newServer = OServerMain.create()
		shutdown
		newServer.startup(xml.toString)
		server = Some(newServer)
	})
	def shutdown = Stopwatch.time("OrientDBEmbeddable.shutdown", () => {
		server.map(s => s.shutdown)
	})
}
