package com.metl.liftAuthenticator

import javax.servlet.http.HttpServletRequest

import net.liftweb.common.{Box, Full, Empty}
import net.liftweb.mocks.MockHttpServletRequest
import net.liftweb.mockweb.MockWeb
import net.liftweb.mockweb.WebSpec
import net.liftweb.http.{LiftRules, LiftRulesMocker, S, SessionVar, RedirectResponse}

import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any, anyInt}

import java.util.concurrent.TimeUnit
import java.io.IOException

import org.junit.runner.RunWith
import org.specs.runner.{JUnitSuiteRunner, JUnit}

@RunWith(classOf[JUnitSuiteRunner])
class CASAuthenticationSpec extends WebSpec with JUnit with MockitoSugar {
}
