package com.metl.utils

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.ActorPing
import net.liftweb.util.Helpers.TimeSpan
import net.liftweb.actor.LiftActor

case object Refresh
class PeriodicallyRefreshingVar[T](acceptedStaleTime:TimeSpan, action:()=>Box[T]) extends LiftActor{
	private var lastResult:Box[T] = Empty
	doGet
	private def doGet:Unit = {
		lastResult = action()
		ActorPing.schedule(this,Refresh,acceptedStaleTime:TimeSpan)
	}
	def get:Box[T] = lastResult
	override def messageHandler = {
		case Refresh => doGet
		case _ => {}
	}
}

class ChangeNotifyingSessionVar[T](dflt: =>T) extends SessionVar[T](dflt){
    private var onChange:List[T=>Unit] = List.empty[T=>Unit]
    override def setFunc(name:String,value:T)={
    		super.setFunc(name,value)
        onChange.foreach(handler=>handler(value))
    }
    def subscribe(handler:T=>Unit)= onChange = handler :: onChange
    def unsubscribe(handler:T=>Unit)= onChange = onChange.filterNot(_ == handler)
}

