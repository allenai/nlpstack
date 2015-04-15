package org.allenai.nlpstack.parse

import org.allenai.common.json._
import org.allenai.nlpstack.parse.poly.core.{ Sentence, Token }
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint
import org.allenai.nlpstack.parse.poly.polyparser.{
  ForbiddenArcLabel,
  ForbiddenEdge,
  PolytreeParse,
  RequestedCpos,
  RequestedArc
}

import spray.json._
import spray.json.DefaultJsonProtocol._

/** spray-json protocol for objects used by the banker UI. This will help ease the migration to
  * reming (or allow the banker UI to stay on spray-json).
  * The banker UI uses TransitionConstraint and PolytreeParse in its models.
  */
/*
object BankerProtocol {
  // TransitionConstraint format.
  implicit val forbiddenEdgeFormat =
    jsonFormat2(ForbiddenEdge.apply).pack("constraintType" -> "forbiddenEdge")

  implicit val forbiddenArcLabelFormat =
    jsonFormat3(ForbiddenArcLabel.apply).pack("constraintType" -> "forbiddenArcLabel")

  implicit val requestedArcFormat =
    jsonFormat(RequestedArc.apply, "token1", "token2", "arcLabel").pack(
      "constraintType" -> "requestedArc"
    )

  implicit val requestedCposFormat =
    jsonFormat2(RequestedCpos.apply).pack("constraintType" -> "requestedCpos")

  implicit object ParserConstraintFormat extends JsonFormat[TransitionConstraint] {
    override def read(jsValue: JsValue): TransitionConstraint = {
      jsValue.asJsObject.unpackWith[TransitionConstraint](
        forbiddenEdgeFormat,
        forbiddenArcLabelFormat,
        requestedArcFormat,
        requestedCposFormat
      )
    }

    override def write(constraint: TransitionConstraint): JsValue = constraint match {
      case forbiddenEdge: ForbiddenEdge => forbiddenEdge.toJson
      case forbiddenArcLabel: ForbiddenArcLabel => forbiddenArcLabel.toJson
      case requestedArc: RequestedArc => requestedArc.toJson
      case requestedCpos: RequestedCpos => requestedCpos.toJson
    }
  }

  // PolytreeParse
  implicit val tokenFormat = jsonFormat2(Token.apply)
  implicit val sentenceFormat = jsonFormat1(Sentence.apply)
  implicit val polytreeParseFormat = jsonFormat4(PolytreeParse.apply)
}
*/
