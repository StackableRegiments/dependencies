package com.metl

import net.liftweb.util._
import net.liftweb.common._
import org.scalatest._
import collection.mutable.Stack

class OfficeConverterSpec extends FlatSpec {

  "A stack" should "pop value in last-in-first-out order" in {
    val stack = new Stack[Int]
    stack.push(1)
    stack.push(2)
    assert(stack.pop() === 2)
    assert(stack.pop() === 1)
  }

  it should "throw NoSuchElementException if an empty stack is popped" in {
    val emptyStack = new Stack[String]
    intercept[NoSuchElementException] {
      emptyStack.pop()
    }
  }
}
