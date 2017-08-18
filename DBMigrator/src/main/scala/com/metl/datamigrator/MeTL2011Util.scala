package com.metl.datamigrator

import java.util.{Date, GregorianCalendar}

object MeTL2011Util {
  /** Convert MeTL2011 timestamp (nanos since 1/1/0000) to Java timestamp (millis since 1/1/1970). */
  def fromTicks(ticks: String):Long = {
    val ticksPerMilli = 10000
    val ticksInMilli = BigInt(ticks) / ticksPerMilli
    val calendar = new GregorianCalendar()
    // Set to C# start time: 1 January 0000
    calendar.set(1,0,0)
    val epochInMilli = calendar.getTimeInMillis * -1
    new Date((ticksInMilli - epochInMilli).toLong).getTime
  }
}
