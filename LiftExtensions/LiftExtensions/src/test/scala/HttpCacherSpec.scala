package com.metl.liftExtensions

import java.util.Date

import org.specs2.{Specification,_}
/*
// from lift
import javax.servlet.http.HttpServletRequest

import scala.xml.NodeSeq

import org.specs2.mutable._

import net.liftweb.common
import net.liftweb.common.Empty
import common.{Box,Empty,Full}
import net.liftweb.http
import http._
import net.liftweb.json
import json.JsonAST._
import net.liftweb.mocks
import mocks.MockHttpServletRequest
import net.liftweb.http.LiftRules
import net.liftweb.http.LiftRulesMocker
import net.liftweb.common.Box
import net.liftweb.http.LiftSession
import net.liftweb.json.JsonAST.JValue
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.mockweb.MockWeb
import net.liftweb.http.Req
import net.liftweb.http.S
import net.liftweb.common.Full
import org.specs2.execute.Result
// end from lift
*/
/*
import org.scalatest.{Matchers,FunSuite,BeforeAndAfter,_}
import org.scalatest.matchers.{HavePropertyMatcher, HavePropertyMatchResult}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.OptionValues._
*/

import net.liftweb.mockweb.MockWeb._
import net.liftweb.mockweb._
import net.liftweb.mockweb.WebSpec
import net.liftweb.mocks.MockHttpServletRequest

import net.liftweb.http._//{S, BasicResponse, InMemoryResponse}
//import net.liftweb.http.SessionVar
import net.liftweb.util.Helpers._

//import org.junit.runner.RunWith
//import org.specs2.runner.{JUnitRunner}//, JUnit}
//import org.specs2.{SpecificationWithJUnit}//, JUnit}


//@RunWith(classOf[JUnitSuiteRunner])
//@RunWith(classOf[JUnitRunner])

object WebSpecBoot {
  def boot() {
    /*
    // Add this so that withTemplateFor test works
    LiftRules.addToPackages("net.liftweb.mockweb")


    LiftRules.statelessRewrite.append {
      case RewriteRequest(ParsePath(List("test", "stateless"), _, _, _), _, _) => {
        RewriteResponse(List("stateless", "works"))
      }
    }

    LiftRules.statefulRewrite.append {
      case RewriteRequest(ParsePath(List("test", "stateful"), _, _, _), _, _) => {
        RewriteResponse(List("stateful", "works"))
      }
    }
    */
  }
}

class HttpCacherSpec extends WebSpec(WebSpecBoot.boot _) {// with SpecificationWithJUnit { //WebSpec with JUnit {

  sequential 

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
        val response = cacher.constructResponse(binary, "image/jpg",new TimeSpan(10 * 1000))// 10 seconds)
        response.code mustEqual 200

        // etag mustEqual binary.checksum throws an error for some reason...
        (extractHeaderValue(response, "ETag") == binary.checksum) mustEqual  true
        (extractHeaderValue(response, "Expires") == "Tue, 16 Oct 2012 10:37:38 +1100") mustEqual true
        (extractHeaderValue(response, "Cache-Control") == "max-age=0, must-revalidate") mustEqual true
      }

    "response of 304 if not modified" withSFor(testUrl) withMods(_.headers = Map(("If-None-Match", List("da39a3ee5e6b4b0d3255bfef95601890afd80709")))) in {
        val binary = CachedBinary(Array.empty[Byte], new Date().getTime)
        val cacher = new HttpCacher
        val response = cacher.constructResponse(binary, "image/jpg", new TimeSpan(10 * 1000))// seconds)
        response.code mustEqual 304
      }
    }
}
