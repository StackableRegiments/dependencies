package com.metl.test

import org.scalatest._
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import org.scalatest.matchers.{ShouldMatchers, HavePropertyMatcher, HavePropertyMatchResult}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.OptionValues._

import net.liftweb.util.Helpers._
import net.liftweb.common._
import scala.xml._
import com.metl.data._
import Privacy._

import net.liftweb.json._
import net.liftweb.json.Serialization.{read, write}

class PrivacySerializer extends net.liftweb.json.Serializer[Privacy] {
    private val PrivacyClass = classOf[Privacy]

    def deserialize(implicit format: Formats): PartialFunction[(TypeInfo, JValue), Privacy] = {
      case (TypeInfo(PrivacyClass, _), json) => json match {
        case JString(p) => Privacy.parse(p)
        case x => throw new MappingException("Can't convert " + x + " to Privacy")
      }
    }

    def serialize(implicit format: Formats): PartialFunction[Any, JValue] = {
      case x: Privacy => JField("privacy", JString(x.toString.toLowerCase))
    }
}

class JsonSerializerSuite extends FunSuite with GeneratorDrivenPropertyChecks with BeforeAndAfter with ShouldMatchers with MeTLMoveDeltaMatchers with MeTLDataGenerators {

	var jsonSerializer: JsonSerializer = _
    implicit val formats = Serialization.formats(NoTypeHints) + new PrivacySerializer

	before {
	  jsonSerializer = new JsonSerializer("empty")
	}

	test("parse metl move delta to json and back") {
      forAll (genMoveDelta) { (genMoveDelta: MeTLMoveDelta) =>

          val json = jsonSerializer.fromMeTLMoveDelta(genMoveDelta)
          val md = jsonSerializer.toMeTLMoveDelta(json)

          md should equal(genMoveDelta)
      }
	}

    test("parse metl ink to json and back") {
      forAll (genInk) { (genInk: MeTLInk) =>

        val json = jsonSerializer.fromMeTLInk(genInk)
        val ink = jsonSerializer.toMeTLInk(json)

        ink should equal(genInk)
      }
    }

    ignore("parse metl image to json and back") {
      forAll (genImage) { (genImage: MeTLImage) =>

        val json = jsonSerializer.fromMeTLImage(genImage)
        val image = jsonSerializer.toMeTLImage(json)
        info("toMeTLImage: Creates Full(Array.empty[Byte]) instead of Empty, and Array instead of WrappedArray")

        image should equal(genImage)
      }
    }

    test("parse metl text to json and back") {
      forAll (genText) { (genText: MeTLText) =>

        val json = jsonSerializer.fromMeTLText(genText)
        val text = jsonSerializer.toMeTLText(json)

        text should equal(genText)
      }
    }

    test("parse metl dirty ink to json and back") {
      forAll (genDirtyInk) { (genDirtyInk: MeTLDirtyInk) =>

        val json = jsonSerializer.fromMeTLDirtyInk(genDirtyInk)
        val dirtyInk = jsonSerializer.toMeTLDirtyInk(json)

        dirtyInk should equal(genDirtyInk)
      }
    }

    test("parse metl dirty image to json and back") {
      forAll (genDirtyImage) { (genDirtyImage: MeTLDirtyImage) =>

        val json = jsonSerializer.fromMeTLDirtyImage(genDirtyImage)
        val dirtyImage = jsonSerializer.toMeTLDirtyImage(json)

        dirtyImage should equal(genDirtyImage)
      }
    }
}
