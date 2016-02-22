package com.metl.h2

import com.metl.utils._
import com.metl.data._
import com.metl.persisted._
import scala.xml._
import net.liftweb.mapper.StandardDBVendor

object LocalH2ServerConfiguration{
	def initialize = {
    ServerConfiguration.addServerConfigurator(LocalH2ServerConfigurator) 
    ServerConfiguration.addServerConfigurator(SqlServerConfigurator) 
  }
}

class LocalH2BackendAdaptor(name:String,filename:Option[String],onConversationDetailsUpdated:Conversation=>Unit) extends PersistedAdaptor(name,"localhost",onConversationDetailsUpdated){
	override lazy val dbInterface = new H2Interface(name,filename,onConversationDetailsUpdated)
	override def shutdown = dbInterface.shutdown
}
object LocalH2ServerConfigurator extends ServerConfigurator{
	override def matchFunction(e:Node) = (e \\ "type").headOption.exists(_.text == "localH2")
  override def interpret(e:Node,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentailsFunc:()=>Tuple2[String,String],conversationListenerCredentialsFunc:()=>Tuple2[String,String],httpCredentialsFunc:()=>Tuple2[String,String]) = {
    Some(new LocalH2BackendAdaptor("localH2",(e \ "filename").headOption.map(_.text),onConversationDetailsUpdated))
  }
}

class SqlBackendAdaptor(name:String,vendor:StandardDBVendor,onConversationDetailsUpdated:Conversation=>Unit) extends PersistedAdaptor(name,"localhost",onConversationDetailsUpdated){
	override lazy val dbInterface = new SqlInterface(name,vendor,onConversationDetailsUpdated)
	override def shutdown = dbInterface.shutdown
}
object SqlServerConfigurator extends ServerConfigurator{
	override def matchFunction(e:Node) = (e \\ "type").headOption.exists(_.text == "sql")
  override def interpret(e:Node,onConversationDetailsUpdated:Conversation=>Unit,messageBusCredentailsFunc:()=>Tuple2[String,String],conversationListenerCredentialsFunc:()=>Tuple2[String,String],httpCredentialsFunc:()=>Tuple2[String,String]) = {
    for (
      driver <- (e \\ "driver").headOption.map(_.text);
      url <- (e \\ "url").headOption.map(_.text)
    ) yield {
      val username = (e \\ "username").headOption.map(_.text)
      val password = (e \\ "password").headOption.map(_.text)
      val allowPoolExpansion = (e \\ "allowPoolExpansion").headOption.exists(_.text.toLowerCase.trim == "true")
      val maxPoolSize = (e \\ "maxPoolSize").headOption.map(_.text.toInt).getOrElse(100)
      val maxExpandedSize = (e \\ "maxExpansion").headOption.map(_.text.toInt).getOrElse(200)
      val vendor = new StandardDBVendor(driver,url,username,password){
        override def allowTemporaryPoolExpansion = allowPoolExpansion
        override def maxPoolSize = maxPoolSize
        override def doNotExpandBeyond = maxExpandedSize
      }
      new SqlBackendAdaptor("sql",vendor,onConversationDetailsUpdated)
    }
  }
}
