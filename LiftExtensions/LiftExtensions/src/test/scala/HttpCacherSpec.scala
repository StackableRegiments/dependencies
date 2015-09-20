package com.metl.liftExtensions

import java.util.Date

import org.specs2.{Specification,_}

import net.liftweb.mockweb.MockWeb._
import net.liftweb.mockweb._
import net.liftweb.mockweb.WebSpec
import net.liftweb.mocks.MockHttpServletRequest

import net.liftweb.http._
import net.liftweb.util.Helpers._

/*
 There's a minor bug in sbt around this WebSpec right now - it's actually running the tests, and declaring a failure if there're failures, but beforehand it's printing output from a testrunner which claims its not running any tests.  In fact everything is working correctly, which is pleasing, but it'd be better if SBT didn't misreport that no tests were run before correctly reporting the correct run (which is a silent report if fully successful).
 */

class HttpCacherSpec extends WebSpec {
  val testUrl1 = "http://test.metl.com/test"

  def extractHeaderValue(response: BasicResponse, header: String): String = {
      
      response.headers find { h => h._1 == header } match {
        case Some(etag) => etag._2
        case _ => "Empty"
      }
  }

  "the HttpCacher" should {
    //args(sequential = true) // this is an example if we were using sessionVars, when we'd want sequential tests to be enforced
    val binary = CachedBinary(Array.empty[Byte], 1350344248511L)

    val cacher = new HttpCacher
    val response = cacher.constructResponse(binary, "image/jpg",new TimeSpan(10 * 1000))// 10 seconds)
    "construct 200 response for empty data" withSFor(testUrl1) in {
      response.code mustEqual 200 
    }

      // etag mustEqual binary.checksum throws an error for some reason...
    "construct appropriate etag for empty data" in {  
     (extractHeaderValue(response, "ETag") == binary.checksum) mustEqual  true
    }  
    "construct appropriate expires header for empty data" in {
     (extractHeaderValue(response, "Expires") == "Tue, 16 Oct 2012 10:37:38 +1100") mustEqual true
    }
   "construct appropriate cache-control for empty data" in {
     (extractHeaderValue(response, "Cache-Control") == "max-age=0, must-revalidate") mustEqual true
    }
    "response of 304 if not modified" withSFor(testUrl1) withMods(_.headers = Map(("If-None-Match", List("da39a3ee5e6b4b0d3255bfef95601890afd80709")))) in {
      val binary = CachedBinary(Array.empty[Byte], new Date().getTime)
      val cacher = new HttpCacher
      val response = cacher.constructResponse(binary, "image/jpg", new TimeSpan(10 * 1000))// seconds)
      response.code mustEqual 304
    }
  }
}
