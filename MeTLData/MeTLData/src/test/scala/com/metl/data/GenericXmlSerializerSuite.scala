package com.metl.data

import org.scalatest._
import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar
import org.scalatest.OptionValues._
import org.scalatest.prop.TableDrivenPropertyChecks._

import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any, anyInt}

class GenericXmlSerializerSuite extends FunSuite with MockitoSugar {

    test("extract privacy of private from content") {
        val content = <ink><color>blue</color><privacy>private</privacy></ink>
        val result = XmlUtils.getPrivacyByName(content, "privacy")

        assert(result === Privacy.PRIVATE) 
    }

    test("extract privacy of not_set from content") {
        val content = <ink><color>blue</color><privacy>not_set</privacy></ink>
        val result = XmlUtils.getPrivacyByName(content, "privacy")

        assert(result === Privacy.NOT_SET) 
    }

    test("extract privacy of public from content") {
        val content = <ink><color>blue</color><privacy>public</privacy></ink>
        val result = XmlUtils.getPrivacyByName(content, "privacy")

        assert(result === Privacy.PUBLIC) 
    }

    test("extract non-existant privacy from content") {
        val content = <ink><color>blue</color></ink>
        val result = XmlUtils.getPrivacyByName(content, "privacy")

        assert(result === Privacy.NOT_SET) 
    }

    test("extract invalid privacy value from privacy element") {
        val content = <ink><color>blue</color><privacy>brown</privacy></ink>
        val result = XmlUtils.getPrivacyByName(content, "privacy")

        assert(result === Privacy.NOT_SET) 
    }

}
