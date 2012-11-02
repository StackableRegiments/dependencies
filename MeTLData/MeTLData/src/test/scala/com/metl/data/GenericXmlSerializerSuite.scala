package com.metl.data

import org.scalatest._
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.{ShouldMatchers, HavePropertyMatcher, HavePropertyMatchResult}
import org.scalatest.mock.MockitoSugar
import org.scalatest.OptionValues._

import org.mockito.Mockito._
import org.mockito.Matchers.{eq => the, any, anyInt}

import net.liftweb.common._
import scala.xml._
import Privacy._

trait MeTLDataMatchers {

    def compareBoxedArray[A](left: Box[Array[A]], right: Box[Array[A]]) : Boolean = 
      (left, right) match {
          case (Empty, Empty) => true  
          case (Full(x), Full(y)) => x.sameElements(y)
          case _ => false
      }

    def server(expectedValue: ServerConfiguration) =
      new HavePropertyMatcher[MeTLStanza, ServerConfiguration] {
        def apply(stanza: MeTLStanza) =
          HavePropertyMatchResult(
            stanza.server == expectedValue,
            "server",
            expectedValue,
            stanza.server
          )
      }

    def author(expectedValue: String) =
      new HavePropertyMatcher[MeTLStanza, String] {
        def apply(stanza: MeTLStanza) =
          HavePropertyMatchResult(
            stanza.author == expectedValue,
            "author",
            expectedValue,
            stanza.author
          )
      }

    def timestamp(expectedValue: Long) =
      new HavePropertyMatcher[MeTLStanza, Long] {
        def apply(stanza: MeTLStanza) =
          HavePropertyMatchResult(
            stanza.timestamp == expectedValue,
            "timestamp",
            expectedValue,
            stanza.timestamp
          )
      }

    def target(expectedValue: String) =
      new HavePropertyMatcher[MeTLCanvasContent, String] {
        def apply(stanza: MeTLCanvasContent) =
          HavePropertyMatchResult(
            stanza.target == expectedValue,
            "target",
            expectedValue,
            stanza.target
          )
      }

    def privacy(expectedValue: Privacy) =
      new HavePropertyMatcher[MeTLCanvasContent, Privacy] {
        def apply(stanza: MeTLCanvasContent) =
          HavePropertyMatchResult(
            stanza.privacy == expectedValue,
            "privacy",
            expectedValue,
            stanza.privacy
          )
      }

    def slide(expectedValue: String) =
      new HavePropertyMatcher[MeTLCanvasContent, String] {
        def apply(stanza: MeTLCanvasContent) =
          HavePropertyMatchResult(
            stanza.slide == expectedValue,
            "slide",
            expectedValue,
            stanza.slide
          )
      }

    def identity(expectedValue: String) =
      new HavePropertyMatcher[MeTLCanvasContent, String] {
        def apply(stanza: MeTLCanvasContent) =
          HavePropertyMatchResult(
            stanza.identity == expectedValue,
            "identity",
            expectedValue,
            stanza.identity
          )
      }

    def scaleFactorX(expectedValue: Double) =
      new HavePropertyMatcher[MeTLCanvasContent, Double] {
        def apply(stanza: MeTLCanvasContent) =
          HavePropertyMatchResult(
            stanza.scaleFactorX == expectedValue,
            "scaleFactorX",
            expectedValue,
            stanza.scaleFactorX
          )
      }

    def scaleFactorY(expectedValue: Double) =
      new HavePropertyMatcher[MeTLCanvasContent, Double] {
        def apply(stanza: MeTLCanvasContent) =
          HavePropertyMatchResult(
            stanza.scaleFactorY == expectedValue,
            "scaleFactorY",
            expectedValue,
            stanza.scaleFactorY
          )
      }

    def tag(expectedValue: String) =
      new HavePropertyMatcher[MeTLImage, String] {
        def apply(stanza: MeTLImage) =
          HavePropertyMatchResult(
            stanza.tag == expectedValue,
            "tag",
            expectedValue,
            stanza.tag
          )
      }

    def source(expectedValue: Box[String]) =
      new HavePropertyMatcher[MeTLImage, Box[String]] {
        def apply(stanza: MeTLImage) =
          HavePropertyMatchResult(
            stanza.source == expectedValue,
            "source",
            expectedValue,
            stanza.source
          )
      }

    def imageBytes(expectedValue: Box[Array[Byte]]) =
      new HavePropertyMatcher[MeTLImage, Box[Array[Byte]]] {
        def apply(stanza: MeTLImage) =
          HavePropertyMatchResult(
            compareBoxedArray(stanza.imageBytes, expectedValue),
            "imageBytes",
            expectedValue,
            stanza.imageBytes
          )
      }

    def pngBytes(expectedValue: Box[Array[Byte]]) =
      new HavePropertyMatcher[MeTLImage, Box[Array[Byte]]] {
        def apply(stanza: MeTLImage) =
          HavePropertyMatchResult(
            compareBoxedArray(stanza.pngBytes, expectedValue),
            "pngBytes",
            expectedValue,
            stanza.pngBytes
          )
      }

    def width(expectedValue: Double) =
      new HavePropertyMatcher[MeTLImage, Double] {
        def apply(stanza: MeTLImage) =
          HavePropertyMatchResult(
            stanza.width == expectedValue,
            "width",
            expectedValue,
            stanza.width
          )
      }

    def height(expectedValue: Double) =
      new HavePropertyMatcher[MeTLImage, Double] {
        def apply(stanza: MeTLImage) =
          HavePropertyMatchResult(
            stanza.height == expectedValue,
            "height",
            expectedValue,
            stanza.height
          )
      }

    def x(expectedValue: Double) =
      new HavePropertyMatcher[MeTLImage, Double] {
        def apply(stanza: MeTLImage) =
          HavePropertyMatchResult(
            stanza.x == expectedValue,
            "x",
            expectedValue,
            stanza.x
          )
      }

    def y(expectedValue: Double) =
      new HavePropertyMatcher[MeTLImage, Double] {
        def apply(stanza: MeTLImage) =
          HavePropertyMatchResult(
            stanza.y == expectedValue,
            "y",
            expectedValue,
            stanza.y
          )
      }

      def inkIds(expectedValue: Seq[String]) =
        new HavePropertyMatcher[MeTLMoveDelta, Seq[String]] {
          def apply(stanza: MeTLMoveDelta) =
            HavePropertyMatchResult(
              stanza.inkIds == expectedValue,
              "inkIds",
              expectedValue,
              stanza.inkIds
            )
        }

      def imageIds(expectedValue: Seq[String]) =
        new HavePropertyMatcher[MeTLMoveDelta, Seq[String]] {
          def apply(stanza: MeTLMoveDelta) =
            HavePropertyMatchResult(
              stanza.imageIds == expectedValue,
              "imageIds",
              expectedValue,
              stanza.imageIds
            )
        }
      def textIds(expectedValue: Seq[String]) =
        new HavePropertyMatcher[MeTLMoveDelta, Seq[String]] {
          def apply(stanza: MeTLMoveDelta) =
            HavePropertyMatchResult(
              stanza.textIds == expectedValue,
              "textIds",
              expectedValue,
              stanza.textIds
            )
        }

    def xTranslate(expectedValue: Double) =
      new HavePropertyMatcher[MeTLMoveDelta, Double] {
        def apply(stanza: MeTLMoveDelta) =
          HavePropertyMatchResult(
            stanza.xTranslate == expectedValue,
            "xTranslate",
            expectedValue,
            stanza.xTranslate
          )
      }

    def yTranslate(expectedValue: Double) =
      new HavePropertyMatcher[MeTLMoveDelta, Double] {
        def apply(stanza: MeTLMoveDelta) =
          HavePropertyMatchResult(
            stanza.yTranslate == expectedValue,
            "yTranslate",
            expectedValue,
            stanza.yTranslate
          )
      }

    def xScale(expectedValue: Double) =
      new HavePropertyMatcher[MeTLMoveDelta, Double] {
        def apply(stanza: MeTLMoveDelta) =
          HavePropertyMatchResult(
            stanza.xScale == expectedValue,
            "xScale",
            expectedValue,
            stanza.xScale
          )
      }

    def yScale(expectedValue: Double) =
      new HavePropertyMatcher[MeTLMoveDelta, Double] {
        def apply(stanza: MeTLMoveDelta) =
          HavePropertyMatchResult(
            stanza.yScale == expectedValue,
            "yScale",
            expectedValue,
            stanza.yScale
          )
      }

    def newPrivacy(expectedValue: Privacy) =
      new HavePropertyMatcher[MeTLMoveDelta, Privacy] {
        def apply(stanza: MeTLMoveDelta) =
          HavePropertyMatchResult(
            stanza.newPrivacy == expectedValue,
            "newPrivacy",
            expectedValue,
            stanza.newPrivacy
          )
      }

    def isDeleted(expectedValue: Boolean) =
      new HavePropertyMatcher[MeTLMoveDelta, Boolean] {
        def apply(stanza: MeTLMoveDelta) =
          HavePropertyMatchResult(
            stanza.isDeleted == expectedValue,
            "isDeleted",
            expectedValue,
            stanza.isDeleted
          )
      }
                          <yTranslate></yTranslate>
                          <xScale></xScale>
                          <yScale></yScale>
                          <newPrivacy></newPrivacy>
                          <isDeleted></isDeleted>
}
/*
        result.tag should equal (expected.tag)
        result.source should equal (expected.source)
        result.imageBytes match {
            case Full(image) => image should equal (expected.imageBytes.openOr(Array.empty[Byte]))
            case _ => println("imageBytes does not match")
        }
        result.pngBytes should be (expected.pngBytes)
        result.width should equal (expected.width)
        result.height should equal (expected.height)
        result.x should equal (expected.x)
        result.y should equal (expected.y)
*/

class GenericXmlSerializerSuite extends FunSuite with MockitoSugar with BeforeAndAfter with ShouldMatchers with MeTLDataMatchers {

    var xmlSerializer: GenericXmlSerializer = _

    before {
      xmlSerializer = new GenericXmlSerializer("empty")
    }

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

    test("extract canvas content") {
        val content = <content>
                          <target>presentationSpace</target> 
                          <privacy>Private</privacy>
                          <slide>3</slide>
                          <identity>eecrole</identity>
                      </content>

        val result = XmlUtils.parseCanvasContent(content)

        assert(result === ParsedCanvasContent("presentationSpace", Privacy.PRIVATE, "3", "eecrole"))
    }

    test("extract different depth canvas content") {
      val content = <conversation>
                      <canvas render="main">
                        <content>
                          Lots of content
                        </content>
                      </canvas>
                      <canvas render="notepad">
                        <identity>eecrole</identity>
                        <target>presentationSpace</target>
                        <privacy>Private</privacy>
                        <slides>
                          <slide>3</slide>
                        </slides>
                      </canvas> 
                      <metadata>
                        <timestamp>334534</timestamp>
                      </metadata>
                    </conversation>

        val result = XmlUtils.parseCanvasContent(content)

        assert(result === ParsedCanvasContent("presentationSpace", Privacy.PRIVATE, "3", "eecrole"))
    }

    test("deconstruct parsed canvas content to xml") {
        val parsed = ParsedCanvasContent("target", Privacy.PUBLIC, "12", "eecrole")

        val result = XmlUtils.parsedCanvasContentToXml(parsed)

        assert(result === List(<target>target</target>, <privacy>public</privacy>, <slide>12</slide>, <identity>eecrole</identity>))
    }

    test("extract metl content from xml") {
        val content = <metldata><author>eecrole</author><metlmetadata><timestamp>234234534634</timestamp></metlmetadata></metldata>

        val result = XmlUtils.parseMeTLContent(content)
        info("timestamp is ignored")
        assert(result === ParsedMeTLContent("eecrole", -1L))
    }

    test("deconstruct metl content to xml") {
        val parsed = ParsedMeTLContent("eecrole", 235245290623L)
        val result = XmlUtils.parsedMeTLContentToXml(parsed)

        info("timestamp is ignored")
        assert(result === List(<author>eecrole</author>))
    }

    test("construct generic xml serializer with empty server configuration") {

       assert(xmlSerializer.config === EmptyBackendAdaptor) 
    }
    
    test("extract metl ink from xml") {

        val content = <message>
                        <ink>
                          <author>eecrole</author>
                          <target>test</target>
                          <privacy>private</privacy>
                          <slide>4</slide>
                          <identity>eecrole:223445834582</identity>
                          <checksum>234235.234234</checksum>
                          <startingSum>233453.1498</startingSum>
                          <points>123.34 234 23</points>
                          <color>#ffff0000</color>
                          <thickness>40.0</thickness>
                          <highlight>false</highlight>
                        </ink>
                      </message>

        val result = xmlSerializer.toMeTLStanza(content)
        assert(result === MeTLInk(ServerConfiguration.empty, "eecrole", -1L, 234235.234234, 233453.1498,List(Point(123.34,234,23)), 
            Color(255, 255, 0, 0), 40.0, false, "test", Privacy.PRIVATE, "4", "eecrole:223445834582"))
    }

    test("extract metl text from xml") {

        val content = <message>
                        <textbox>
                          <author>eecrole</author>
                          <target>test</target>
                          <privacy>private</privacy>
                          <slide>4</slide>
                          <identity>eecrole:223445834582</identity>
                          <tag>eecrole:223445834582</tag>
                          <caret>0</caret>
                          <text>Hello World!</text>
                          <style>Underline</style>
                          <family>Helvetica</family>
                          <weight>Bold</weight>
                          <size>12.0</size>
                          <decoration>Italics</decoration>
                          <color>#ffff0000</color>
                          <width>200</width>
                          <height>100</height>
                          <x>120</x>
                          <y>300</y>
                        </textbox>
                      </message>

        val result = xmlSerializer.toMeTLStanza(content)
        assert(result === MeTLText(ServerConfiguration.empty, "eecrole", -1L, "Hello World!", 100.0, 200.0, 0, 120.0, 300.0, "eecrole:223445834582",
            "Underline", "Helvetica", "Bold", 12.0, "Italics", "eecrole:223445834582", "test", Privacy.PRIVATE, "4", Color(255, 255, 0, 0)))
    }

    test("extract metl image from xml") {

        val content = <message>
                        <image>
                          <author>eecrole</author>
                          <target>test</target>
                          <privacy>private</privacy>
                          <slide>4</slide>
                          <identity>metlImage</identity>
                          <tag>eecrole:223445834582</tag>
                          <source>http://test.metl.com/test/testimg23435.png</source>
                          <width>200</width>
                          <height>100</height>
                          <x>120</x>
                          <y>300</y>
                        </image>
                      </message>

        val result = xmlSerializer.toMeTLStanza(content).asInstanceOf[MeTLImage]

        result should have (
            server (ServerConfiguration.empty),
            author ("eecrole"),
            timestamp (-1L),
            tag ("eecrole:223445834582"),
            source (Full("http://test.metl.com/test/testimg23435.png")),
            imageBytes (Full(Array.empty[Byte])),
            pngBytes (Empty),
            width (200.0),
            height (100.0),
            x (120.0),
            y (300.0),
            target ("test"),
            privacy (Privacy.PRIVATE),
            slide ("4"),
            identity ("metlImage")
        )
    }

    test("extract metl dirty ink from xml") {

        val content = <message>
                        <dirtyInk>
                          <author>eecrole</author>
                          <target>test</target>
                          <privacy>public</privacy>
                          <slide>4</slide>
                          <identity>metlDirtyInk</identity>
                        </dirtyInk>
                      </message>

        val result = xmlSerializer.toMeTLStanza(content).asInstanceOf[MeTLDirtyInk]

        result should have (
            server (ServerConfiguration.empty),
            author ("eecrole"),
            timestamp (-1L),
            target ("test"),
            privacy (Privacy.PUBLIC),
            slide ("4"),
            identity ("metlDirtyInk")
        )
    }

    test("extract metl dirty text from xml") {

        val content = <message>
                        <dirtyText>
                          <author>eecrole</author>
                          <target>test</target>
                          <privacy>public</privacy>
                          <slide>4</slide>
                          <identity>metlDirtyText</identity>
                        </dirtyText>
                      </message>

        val result = xmlSerializer.toMeTLStanza(content).asInstanceOf[MeTLDirtyText]

        result should have (
            server (ServerConfiguration.empty),
            author ("eecrole"),
            timestamp (-1L),
            target ("test"),
            privacy (Privacy.PUBLIC),
            slide ("4"),
            identity ("metlDirtyText")
        )
    }

    test("extract metl dirty image from xml") {

        val content = <message>
                        <dirtyImage>
                          <author>eecrole</author>
                          <target>test</target>
                          <privacy>public</privacy>
                          <slide>4</slide>
                          <identity>metlDirtyImage</identity>
                        </dirtyImage>
                      </message>

        val result = xmlSerializer.toMeTLStanza(content).asInstanceOf[MeTLDirtyImage]

        result should have (
            server (ServerConfiguration.empty),
            author ("eecrole"),
            timestamp (-1L),
            target ("test"),
            privacy (Privacy.PUBLIC),
            slide ("4"),
            identity ("metlDirtyImage")
        )
    }

    test("extract metl move delta from xml") {

        val content = <message>
                        <moveDelta>
                          <author>eecrole</author>
                          <target>test</target>
                          <privacy>public</privacy>
                          <slide>4</slide>
                          <identity>metlMoveDelta</identity>
                          <inkIds>
                            <inkId>1</inkId>
                            <inkId>2</inkId>
                            <inkId>3</inkId>
                          </inkIds>
                          <textIds>
                            <textId>4</textId>
                            <textId>5</textId>
                            <textId>6</textId>
                          </textIds>
                          <imageIds>
                            <imageId>7</imageId>
                            <imageId>8</imageId>
                            <imageId>9</imageId>
                          </imageIds>
                          <xTranslate>142.4</xTranslate>
                          <yTranslate>265.2</yTranslate>
                          <xScale>2.0</xScale>
                          <yScale>4.0</yScale>
                          <newPrivacy>private</newPrivacy>
                          <isDeleted>false</isDeleted>
                        </moveDelta>
                      </message>

        val result = xmlSerializer.toMeTLStanza(content).asInstanceOf[MeTLMoveDelta]

        result should have (
            server (ServerConfiguration.empty),
            author ("eecrole"),
            timestamp (-1L),
            target ("test"),
            privacy (Privacy.PUBLIC),
            slide ("4"),
            identity ("metlMoveDelta"),
            inkIds (Seq("1", "2", "3")),
            textIds (Seq("4", "5", "6")),
            imageIds (Seq("7", "8", "9")),
            xTranslate (142.4),
            yTranslate (265.2),
            xScale (2.0),
            yScale (4.0),
            newPrivacy (Privacy.PRIVATE),
            isDeleted (false)
        )
    }

    test("extract metl quiz from xml") (pending)
}
