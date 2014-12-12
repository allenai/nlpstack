package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.common.json._

import org.allenai.nlpstack.parse.poly.polyparser.{PolytreeParse}
import spray.json.DefaultJsonProtocol._
import spray.json._

/** A Sculpture is a structured output corresponding to a final state of a finite-state
  * machine, whose goal is to transform an unstructured input (a MarbleBlock) into a
  * structured output.
  *
  * As an example, consider a transition-based parser. A MarbleBlock would be a sentence to be
  * parsed, whereas a Sculpture would be a parse tree for that sentence.
  */
trait Sculpture


object Sculpture {

  /** Boilerplate code to serialize a Sculpture to JSON using Spray.
    *
    * NOTE: If a subclass has a field named `type`, this will fail to serialize.
    *
    * NOTE: IF YOU INHERIT FROM ClassificationTask, THEN YOU MUST MODIFY THESE SUBROUTINES
    * IN ORDER TO CORRECTLY EMPLOY JSON SERIALIZATION FOR YOUR NEW SUBCLASS.
    */
  implicit object SculptureJsonFormat extends RootJsonFormat[Sculpture] {
    implicit val polytreeParseFormat =
      jsonFormat4(PolytreeParse.apply).pack("type" -> "PolytreeParse")

    def write(sculpture: Sculpture): JsValue = sculpture match {
      case polytreeParse: PolytreeParse => polytreeParse.toJson
    }

    def read(value: JsValue): Sculpture = value.asJsObject.unpackWith(
      polytreeParseFormat)
  }

}
