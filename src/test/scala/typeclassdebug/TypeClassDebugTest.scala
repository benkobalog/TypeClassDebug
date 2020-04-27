package typeclassdebug

import java.time.DayOfWeek

import io.circe.Json
import org.scalatest.matchers.should.Matchers

class TypeClassDebugTest extends org.scalatest.wordspec.AnyWordSpec with Matchers {

  import TypeClassDebug._
  import io.circe.{Decoder, Encoder}

  "findMissingInstance" should {
    "compile when the type has an instance" in {
      case class ExistingInstances(x: Int, s: String)

      "findMissingInstances[Encoder, ExistingInstances]" should compile
      "findMissingInstances[Decoder, ExistingInstances]" should compile
    }

    "not compile when there's a missing instance at top level" in {
      case class MissingInstance(x: Int, d: java.time.DayOfWeek)

      "findMissingInstances[Encoder, MissingInstance]" shouldNot compile
      "findMissingInstances[Decoder, MissingInstance]" shouldNot compile
    }

    "not compile when there's a missing instance in one of the fields" in {
      case class TopLevel(s: String, m: MissingInstance)
      case class MissingInstance(x: Int, d: java.time.DayOfWeek)

      "findMissingInstances[Encoder, TopLevel]" shouldNot compile
      "findMissingInstances[Decoder, TopLevel]" shouldNot compile
    }

    "only compile when all instances exist" in {
      case class TopLevel(s: String, m: MissingInstance)
      case class MissingInstance(x: Int, d: java.time.DayOfWeek)

      implicit val dayOfWeekEncoder: Encoder[DayOfWeek] = d => Json.fromString(d.getValue.toString)

      "findMissingInstances[Encoder, TopLevel]" should compile
      "findMissingInstances[Decoder, TopLevel]" shouldNot compile
    }
  }
}
