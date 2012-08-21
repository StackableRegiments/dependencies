package com.metl.model
import java.util.Date
import net.liftweb.common._
import net.liftweb.actor._
/*
import net.liftweb.record.field._
import net.liftweb.mongodb._
import net.liftweb.record.field._
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
*/

case class TimerResult(label:String,startTime:Long,duration:Long)
/*
class TimerResultRecord extends MongoRecord[TimerResultRecord] with MongoId[TimerResultRecord]{
	def meta = TimerResultRecord

	object label extends StringField(this,50)
	object startTime extends LongField(this)
	object duration extends LongField(this)
}

object TimerResultRecord extends TimerResultRecord with MongoMetaRecord[TimerResultRecord]{
	def fromTimerResult(r:TimerResult) ={
		val label = r.label
		val startTime = r.startTime
		val duration = r.duration
		createRecord.label(label).startTime(startTime).duration(duration)
	}
}
*/
object StopwatchDBActor extends LiftActor {
	override def messageHandler ={
		case r@TimerResult(label,start,duration) => {
//			TimerResultRecord.fromTimerResult(r).save
			if (duration > 1)
				println("(%sms) %s".format(duration,label))
		}
		case _ => println("StopwatchDBActor received unknown message")
	}
}

object Stopwatch{
	def start(label:String) ={
		val zero = new Date().getTime
		()=>{
			val elapsed = new Date().getTime - zero
			StopwatchDBActor ! TimerResult(label,zero,elapsed)
		}
	}
	def time[T](label:String,action:()=>T) = {
		val timer = Stopwatch.start(label)
		val result = action()
		timer()
		result
		//action()
	}
}
