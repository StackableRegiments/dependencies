package com.metl.datamigrator

import java.time.{Instant, ZoneId, ZonedDateTime}
import java.util.Date

import org.scalatest.FunSuite

class DateTimeUtilSuite extends FunSuite {
  private val zoneId: ZoneId = ZoneId.of("America/New_York")
  private val upper = Instant.from(ZonedDateTime.of(2017,5,30,0,0,0,0,zoneId)).toEpochMilli // 30 June, 2017
  private val lower = Instant.from(ZonedDateTime.of(2017,1,28,0,0,0,0,zoneId)).toEpochMilli // 28 Feb. 2017

  test("date within bounds should not be altered") {
    val input = Instant.from(ZonedDateTime.of(2017,3,1,0,0,0,0,zoneId)).toEpochMilli // April 1, 2017
    val result = DateTimeUtil.getBoundedDateTime(input, Some(upper), Some(lower))
    assert(result == input,"got %s, was expecting %s".format(new Date(result),new Date(input)))
  }
  test("date just below bounds should be set to lower") {
    val input = Instant.from(ZonedDateTime.of(2017,1,27,11,59,59,999999,zoneId)).toEpochMilli // Feb 27, 2017 11:59:59:000000
    val result = DateTimeUtil.getBoundedDateTime(input, Some(upper), Some(lower))
    assert(result == lower,"got %s, was expecting %s".format(new Date(result),new Date(lower)))
  }
  test("date way below bounds should be set to lower") {
    val input = Instant.from(ZonedDateTime.of(2017,1,1,0,0,0,0,zoneId)).toEpochMilli // Feb 1, 2017
    val result = DateTimeUtil.getBoundedDateTime(input, Some(upper), Some(lower))
    assert(result == lower,"got %s, was expecting %s".format(new Date(result),new Date(lower)))
  }
  test("date way above bounds should be set to upper") {
    val input = Instant.from(ZonedDateTime.of(2017,11,1,0,0,0,0,zoneId)).toEpochMilli // Dec 1, 2017
    val result = DateTimeUtil.getBoundedDateTime(input, Some(upper), Some(lower))
    assert(result == upper,"got %s, was expecting %s".format(new Date(result),new Date(upper)))
  }
  test("date just above bounds should be set to upper") {
    val input = Instant.from(ZonedDateTime.of(2017,5,30,0,0,0,1,zoneId)).toEpochMilli // Jun 30, 2017 0:0:0.000001
    val result = DateTimeUtil.getBoundedDateTime(input, Some(upper), Some(lower))
    assert(result == upper,"got %s, was expecting %s".format(new Date(result),new Date(upper)))
  }
  test("date with no bounds should return input") {
    val input = Instant.from(ZonedDateTime.of(2017,5,30,0,0,0,1,zoneId)).toEpochMilli // Jun 30, 2017 0:0:0.000001
    val result = DateTimeUtil.getBoundedDateTime(input, None, None)
    assert(result == input,"got %s, was expecting %s".format(new Date(result),new Date(input)))
  }
}
