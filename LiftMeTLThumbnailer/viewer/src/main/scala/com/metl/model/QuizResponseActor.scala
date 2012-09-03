package com.metl.model

import net.liftweb.actor._
import net.liftweb.common._

case class QuizResponseMessage(jid:String,qr:MeTLQuizResponse)

object EmptyQuizResponseActor extends QuizResponseActor("empty"){
	override def messageHandler = {
		case _ => {}
	}
}

class QuizResponseActor(configName:String) extends LiftActor {
	lazy val config = ServerConfiguration.configForName(configName)
	override def messageHandler ={
		case qrm:QuizResponseMessage => {
			val mb = config.getMessageBus(new MessageBusDefinition(qrm.jid,"singleUseBus"))
			mb.sendStanzaToRoom(qrm.qr)
			mb.release 
		}
		case _ => println("QuizResponseActor received unknown message")
	}
}
