package com.metl.data

import org.scalatest._
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.{ShouldMatchers, HavePropertyMatcher, HavePropertyMatchResult}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.OptionValues._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import scala.xml._
import Privacy._

class MeTLCommandSuite extends FunSuite with GeneratorDrivenPropertyChecks with BeforeAndAfter with ShouldMatchers with MeTLDataGenerators with QueryXml with MeTLCommandMatchers {

	var xmlSerializer: GenericXmlSerializer = _

	before {
	  xmlSerializer = new GenericXmlSerializer("empty")
	}

	test("extract command from xml") {

		val content = <message>
						<body>
							<author>eecrole</author>
							<command>GO_TO_SLIDE</command>
							<parameters>
								<parameter>2234234</parameter>
							</parameters>
						</body>
					  </message>

		val result = xmlSerializer.toMeTLStanza(content).asInstanceOf[MeTLCommand]

		result should have (
			server (ServerConfiguration.empty),
			author ("eecrole"),
			timestamp (-1L),
			command ("GO_TO_SLIDE"),
			commandParameters (List("2234234"))
		)
	}

    ignore("serialize MeTLCommand to xml") {
        forAll (genCommand) { (genCommand: MeTLCommand) =>

            implicit val xml = xmlSerializer.fromMeTLCommand(genCommand)

            genCommand should have (
               author (queryXml[String]("author")),
               command (queryXml[String]("command")),
               commandParameters (XmlUtils.getListOfStringsByNameWithin(xml, "parameter", "parameters"))
            )
        }
    }
}
