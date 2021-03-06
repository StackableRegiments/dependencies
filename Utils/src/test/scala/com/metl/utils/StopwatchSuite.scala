 package com.metl.utils

import org.scalatest._
import org.scalatest.concurrent.AsyncAssertions
import org.scalatest.matchers.MustMatchers

class StopwatchSuite extends FunSuite with AsyncAssertions with MustMatchers {

    test("stopwatch times action") {

      val w = new Waiter

      Stopwatch.time("empty lambda", {
        w.dismiss
      })

      w.await
    }

    test("stopwatch actor ignores non-TimerResult message") {
       
      val timeout = 50
      StopwatchActor !? (timeout, "hello there")
    }

    test("stopwatch returns action's result") {

        val result = Stopwatch.time("timed result of addition", {
            val sum = 2 + 2
            sum
          })

        assert(result === 4)
    }
}
