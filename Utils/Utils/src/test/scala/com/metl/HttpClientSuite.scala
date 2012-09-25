package com.metl

import net.liftweb.util._
import net.liftweb.common._

import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.mock.MockitoSugar
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.concurrent.AsyncAssertions
import org.scalatest.OptionValues._

import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any}

import org.apache.http.conn.ClientConnectionManager

import com.metl.utils._ 

class HttpClientSuite extends FunSuite with AsyncAssertions with ShouldMatchers with MockitoSugar with BeforeAndAfter {

    var connectionManager : ClientConnectionManager = _ 

    before {
        connectionManager = mock[ClientConnectionManager]   
    }

    test("create clean http client") {

        var client = new CleanHttpClient(connectionManager)
    }

    test("shutdown clean http client") {
        
        var client = new CleanHttpClient(connectionManager)

        client.getConnectionManager.shutdown

        verify(connectionManager).shutdown
    }
}
