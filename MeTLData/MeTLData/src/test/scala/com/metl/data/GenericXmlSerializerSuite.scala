package com.metl.data

import org.scalatest._
import org.scalatest.FunSuite
import org.scalatest.mock.MockitoSugar
import org.scalatest.OptionValues._
import org.scalatest.prop.TableDrivenPropertyChecks._

import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any, anyInt}

import scala.xml._

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

    test("extract color as hexstring from specified element name") {
        val content = <ink><color>#0fafbfcf</color><privacy>private</privacy></ink>
        val result = XmlUtils.getColorByName(content, "color")

        assert(result === Color(0x0f, 0xaf, 0xbf, 0xcf))
    }

    test("extract color as long hexstring from specified element name") {
        val content = <ink><color>#0fafbfcf7b</color><privacy>private</privacy></ink>
        val result = XmlUtils.getColorByName(content, "color")

        assert(result === Color.default)
    }

    test("extract color as 3 numbers from content") {
        val content = <ink><blah><color>240 250 128</color></blah><privacy>private</privacy></ink>
        val result = XmlUtils.getColorByName(content, "color")

        assert(result === Color(0xff, 240, 250, 128))
    }

    test("extract color as 4 numbers from content") {
        val content = <ink><blah><color>240 250 128 120</color></blah><privacy>private</privacy></ink>
        val result = XmlUtils.getColorByName(content, "color")

        assert(result === Color(120, 240, 250, 128))
    }

    test("extract non-existant color element from content") {
        val content = <ink><elephant>african</elephant><privacy>private</privacy></ink>
        val result = XmlUtils.getColorByName(content, "color")

        assert(result === Color.default)
    }

    test("extract invalid color from content") {
        val content = <ink><color>mango</color><privacy>private</privacy></ink>
        val result = XmlUtils.getColorByName(content, "color")

        assert(result === Color.default)
    }

    test("extract 4 number color outside of range") {
        val content = <ink><blah><color>240 400 128 120</color></blah><privacy>private</privacy></ink>
        val result = XmlUtils.getColorByName(content, "color")

        assert(result === Color(120, 240, 255, 128))
    }

    test("extract 3 number color outside of range") {
        val content = <ink><blah><color>240 400 128</color></blah><privacy>private</privacy></ink>
        val result = XmlUtils.getColorByName(content, "color")

        assert(result === Color(255, 240, 255, 128))
    }

    test("extract string value from specified element") {
        val content = <ink><elephant>african</elephant><privacy>private</privacy></ink>
        val result = XmlUtils.getStringByName(content, "elephant")

        assert(result === "african")
    }

    test("extract string value from non-existant specified element") {
        val content = <ink><elephant>african</elephant><privacy>private</privacy></ink>
        val result = XmlUtils.getStringByName(content, "tree")

        assert(result === "")
    }

    test("extract true boolean value from specified element") {
        val content = <ink><isHighlighter>true</isHighlighter><privacy>private</privacy></ink>
        val result = XmlUtils.getBooleanByName(content, "isHighlighter")

        assert(result === true)
    }

    test("extract TRUE boolean value from specified element") {
        val content = <ink><isHighlighter>TRUE</isHighlighter><privacy>private</privacy></ink>
        val result = XmlUtils.getBooleanByName(content, "isHighlighter")

        assert(result === true)
    }

    test("extract TrUe boolean value from specified element") {
        val content = <ink><isHighlighter>TrUe</isHighlighter><privacy>private</privacy></ink>
        val result = XmlUtils.getBooleanByName(content, "isHighlighter")

        assert(result === true)
    }

    test("extract false boolean value from specified element") {
        val content = <ink><isHighlighter>false</isHighlighter><privacy>private</privacy></ink>
        val result = XmlUtils.getBooleanByName(content, "isHighlighter")

        assert(result === false)
    }

    test("extract FALSE boolean value from specified element") {
        val content = <ink><isHighlighter>FALSE</isHighlighter><privacy>private</privacy></ink>
        val result = XmlUtils.getBooleanByName(content, "isHighlighter")

        assert(result === false)
    }

    test("extract invalid boolean value from specified element") {
        val content = <ink><isHighlighter>termites</isHighlighter><privacy>private</privacy></ink>
        val result = XmlUtils.getBooleanByName(content, "isHighlighter")

        assert(result === false)
    }

    test("extract negative double value from specified element") {
        val content = <ink><coordX>-123434.02345</coordX></ink>
        val result = XmlUtils.getDoubleByName(content, "coordX")

        assert(result === -123434.02345)
    }
  
    test("extract positive double value from specified element") {
        val content = <ink><coordX>434.045</coordX></ink>
        val result = XmlUtils.getDoubleByName(content, "coordX")

        assert(result === 434.045)
    }

    test("extract invalid double value from specified element") {
        val content = <ink><coordX>pants</coordX></ink>
        val result = XmlUtils.getDoubleByName(content, "coordX")

        assert(result === -1D)
    }

    test("extract negative long value from specified element") {
        val content = <ink><timestamp>-142345224502350203</timestamp></ink>
        val result = XmlUtils.getLongByName(content, "timestamp")

        assert(result === -142345224502350203L)
    }
  
    test("extract positive long value from specified element") {
        val content = <ink><timestamp>92387434597823495</timestamp></ink>
        val result = XmlUtils.getLongByName(content, "timestamp")

        assert(result === 92387434597823495L)
    }

    test("extract invalid long value from specified element") {
        val content = <ink><timestamp>pants</timestamp></ink>
        val result = XmlUtils.getLongByName(content, "timestamp")

        assert(result === -1L)
    }

    test("extract negative int value from specified element") {
        val content = <ink><id>-1234235</id></ink>
        val result = XmlUtils.getIntByName(content, "id")

        assert(result === -1234235)
    }
  
    test("extract positive int value from specified element") {
        val content = <ink><id>345377</id></ink>
        val result = XmlUtils.getIntByName(content, "id")

        assert(result === 345377)
    }

    test("extract invalid int value from specified element") {
        val content = <ink><id>pants</id></ink>
        val result = XmlUtils.getIntByName(content, "id")

        assert(result === -1)
    }

    test("extract list of strings by name within container") {
        val content = <ink>
                        <strokes>
                          <strokeId>1</strokeId>
                          <strokeId>2</strokeId>
                          <strokeId>3</strokeId>
                          <strokeId>4</strokeId>
                          <strokeId>5</strokeId>
                        </strokes>
                      </ink>

       val result = XmlUtils.getListOfStringsByNameWithin(content, "strokeId", "strokes") 

       assert(result === List("1", "2", "3", "4", "5"))
    }

    test("extract value of element by name") {
        val content = <ink><coordX>pants</coordX></ink>
        val result = XmlUtils.getValueOfNode(content, "coordX")

        assert(result === "pants")
    }

    test("extract value of element by name returns only first value") {
        val content = <ink><coordX>pants</coordX><coordX>hats</coordX><coordX>shirts</coordX></ink>
        val result = XmlUtils.getValueOfNode(content, "coordX")

        assert(result === "pants")
    }

    test("extract xml by name") {
        val content = <ink><coordX>pants</coordX></ink>
        val result = XmlUtils.getXmlByName(content, "coordX")

        assert(result.toString === <coordX>pants</coordX>.toString)
    }

    test("extract xml deep by name") {
        val content = <ink><id>2345</id><attributes><coordX>345</coordX><isHighlighter>false</isHighlighter><color>blue</color></attributes></ink>
        val result = XmlUtils.getXmlByName(content, "attributes")

        assert(result.toString === <attributes><coordX>345</coordX><isHighlighter>false</isHighlighter><color>blue</color></attributes>.toString)
    }

    test("extract non-existant xml by name") {
        val content = <something>some value</something>
        val result = XmlUtils.getXmlByName(content, "cat")

        assert(result === NodeSeq.Empty)
    }

    test("extract attribute of node") {
        val content = <ink><color tip="true">black</color></ink>
        val result = XmlUtils.getAttributeOfNode(content, "color", "tip")

        assert(result === "true")
    }

    test("extract non-existant attribute of node") {
        val content = <ink><color alpha="50%">black</color></ink>
        val result = XmlUtils.getAttributeOfNode(content, "color", "tip")

        assert(result === "")
    }
}
