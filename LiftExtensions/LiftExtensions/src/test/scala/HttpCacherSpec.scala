package com.metl.liftExtensions

import java.util.Date

import net.liftweb.mockweb.MockWeb._
import net.liftweb.mockweb.WebSpec
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.http.{S, BasicResponse, InMemoryResponse}
import net.liftweb.http.SessionVar
import net.liftweb.util.Helpers._

import org.junit.runner.RunWith
import org.specs.runner.{JUnitSuiteRunner, JUnit}


@RunWith(classOf[JUnitSuiteRunner])
class HttpCacherSpec extends WebSpec with JUnit {

    val testUrl = "http://test.metl.com/test"

    def extractHeaderValue(response: BasicResponse, header: String): String = {
        
        response.headers find { h => h._1 == header } match {
          case Some(etag) => etag._2
          case _ => "Empty"
        }
    }

    "the HttpCacher" should {

        "construct response for empty data" withSFor(testUrl) in { 

            val binary = CachedBinary(Array.empty[Byte], 1350344248511L)

            val cacher = new HttpCacher
            val response = cacher.constructResponse(binary, "image/jpg", 10 seconds)
            response.code mustEqual 200

            // etag mustEqual binary.checksum throws an error for some reason...
            (extractHeaderValue(response, "ETag") == binary.checksum) mustEqual true
            (extractHeaderValue(response, "Expires") == "Tue, 16 Oct 2012 10:37:38 +1100") mustEqual true
            (extractHeaderValue(response, "Cache-Control") == "max-age=0, must-revalidate") mustEqual true
        }

        "response of 304 if not modified" withSFor(testUrl) withMods(_.headers = Map(("If-None-Match", List("da39a3ee5e6b4b0d3255bfef95601890afd80709")))) in {

            val binary = CachedBinary(Array.empty[Byte], new Date().getTime)

            val cacher = new HttpCacher
            val response = cacher.constructResponse(binary, "image/jpg", 10 seconds)
            response.code mustEqual 304
        }
    }
}
