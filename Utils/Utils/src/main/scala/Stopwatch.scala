package com.metl.utils
import java.util.Date
import net.liftweb.actor._

case class TimerResult(label:String,startTime:Long,duration:Long)

object StopwatchActor extends LiftActor {
	override def messageHandler ={
		case r@TimerResult(label,start,duration) => {
			if (duration > 1)
				println("(%sms) %s".format(duration,label))
		}
		case _ => println("StopwatchActor received unknown message")
	}
}

object Stopwatch{
	private def start(label:String) ={
		val zero = new Date().getTime
		()=>{
			val elapsed = new Date().getTime - zero
			StopwatchActor ! TimerResult(label,zero,elapsed)
		}
	}
	def time[T](label:String,action:()=>T) = {
		val timer = Stopwatch.start(label)
		val result = action()
		timer()
		result
	}
}
