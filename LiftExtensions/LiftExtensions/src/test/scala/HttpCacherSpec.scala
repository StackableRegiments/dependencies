package com.metl.liftExtensions

import java.util.Date

import net.liftweb.mockweb.MockWeb._
import net.liftweb.mockweb.WebSpec
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.http.S
import net.liftweb.http.SessionVar

import org.junit.runner.RunWith
import org.specs.runner.{JUnitSuiteRunner, JUnit}


@RunWith(classOf[JUnitSuiteRunner])
class HttpCacherSpec extends WebSpec with JUnit {

    val testUrl = "http://test.metl.com/test"

    "the HttpCacher" should {
        "construct response for empty data" withSFor(testUrl) in { 

            val binary = CachedBinary(Array.empty[Byte], new Date().getTime)

            val cacher = new HttpCacher
            val response = cacher.constructResponse(binary, "image/jpg", binary.createTime + 1)
            response.code mustEqual 200
        }
    }
}
