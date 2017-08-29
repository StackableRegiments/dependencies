package com.metl.datamigrator

import java.util.Date

import net.liftweb.common.Logger

object DateTimeUtil extends Logger {
  def getBoundedDateTime(input: Long, upper: Option[Long], lower: Option[Long]): Long = {
    var result = lower match {
      case Some(l) => {
        val max = List(input, l).max
        if (max != input) debug("Restricting input date %s to lower bound %s".format(new Date(input), new Date(l)))
        max
      }
      case _ => input
    }
    result = upper match {
      case Some(u) => {
        val min = List(result, u).min
        if( min != result) debug("Restricting input date %s to upper bound %s".format(new Date(result), new Date(u)))
        min
      }
      case _ => result
    }
    result
  }
}
