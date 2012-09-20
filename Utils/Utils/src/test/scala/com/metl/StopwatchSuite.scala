package com.metl

import net.liftweb.util._
import net.liftweb.common._
import org.scalatest.FunSuite
import collection.mutable.Stack

class Stopwatch extends FunSuite {

    test(" test stop watch pop is invoked on a non-empty stack") {

      val stack = new Stack[Int]
      stack.push(1)
      stack.push(2)
      val oldSize = stack.size
      val result = stack.pop
      assert(result === 2)
      assert(stack.size === oldSize - 1)
    }

    test("test stopwatch pop is invoked on an empty stack") {
        
        val emptyStack = new Stack[Int]
        intercept[NoSuchElementException] {
          emptyStack.pop
        }
        assert(emptyStack.isEmpty)
    }
}
