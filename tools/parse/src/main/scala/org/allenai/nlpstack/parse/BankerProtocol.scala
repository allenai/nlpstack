package org.allenai.nlpstack.parse

import org.allenai.common.json._
import org.allenai.nlpstack.parse.poly.core.{ Sentence, Token }
import org.allenai.nlpstack.parse.poly.fsm.TransitionConstraint
import org.allenai.nlpstack.parse.poly.polyparser._

import spray.json._
import spray.json.DefaultJsonProtocol._

/** Interface between the Banker tool and the Polyparser, including a
  * spray-json protocol for objects used by the banker UI. This will help ease the migration to
  * reming (or allow the banker UI to stay on spray-json).
  * The banker UI uses TransitionConstraint and PolytreeParse in its models.
  */
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
  implicit val bankerParseFormat = jsonFormat4(BankerParse.apply)

  /** Tokenizes, tags, and parses an untokenized sentence.
    *
    * @param parser the parser to use for parsing the sentence
    * @param text the untokenized sentence
    * @return a parse for the argument sentence
    */
  def parseUntokenizedSentence(parser: TransitionParser, text: String): Option[BankerParse] = {
    parser.parse(Sentence(Parser.tokenizeSentence(text).toIndexedSeq)) map { parse =>
      BankerParse.fromPolytreeParse(parse)
    }
  }

  /** Parses a sentence subject to a set of constraints.
    *
    * @param parser the parser to use
    * @param sentence the sentence to parse
    * @param constraints the constraints you want to parser to obey
    * @return a parse tree obeying your specified constraints
    */
  def parseWithConstraints(
    parser: TransitionParser,
    sentence: Sentence,
    constraints: Set[TransitionConstraint]
  ): Option[BankerParse] = {

    parser.parse(sentence, constraints) map { parse => BankerParse.fromPolytreeParse(parse) }
  }
}

/** The legacy parse format used by the Banker.
  *
  * It has four major fields:
  * - `tokens` is a vector of Token objects (in the order that they appear in the associated
  * sentence). The zeroth element is assumed to be the nexus.
  * - `breadcrumb` tells you the unique neighbor that is closer to the nexus in the
  * undirected tree (this can be the nexus itself); for instance, if breadcrumb(5) = 3,
  * then token 3 is one step closer to the nexus from token 5. The breadcrumb of the nexus
  * should be -1.
  * - `children` tells you the set of children of a node in the polytree; for instance, if
  * children(5) = Set(3,6,7), then token 5 has three children: tokens 3, 6, and 7
  * - `arclabels` tells you the labeled neighbors of a node in the undirected tree; for instance,
  * if arclabels(5) = Set((4, 'det), (7, 'amod)), then token 5 has two neighbors, reached with
  * arcs labeled 'det and 'amod (the labels are scala Symbol objects)
  *
  * @param sentence the parsed sentence (the zeroth token of which should be the nexus)
  * @param breadcrumb the breadcrumb of each token (see above definition)
  * @param children the set of children of each token in the polytree
  * @param arclabels the set of labeled neighbors of each token in the undirected tree
  */
case class BankerParse(
    sentence: Sentence,
    breadcrumb: Vector[Int],
    children: Vector[Set[Int]],
    arclabels: Vector[Set[(Int, Symbol)]]
) {

  def toPolytreeParse: PolytreeParse = {
    PolytreeParse(sentence, breadcrumb, children,
      arclabels map { labelset =>
        val result: Set[(Int, ArcLabel)] = labelset map {
          case (index, label) =>
            (index, SingleSymbolArcLabel(label))
        }
        result
      })
  }

}

object BankerParse {

  /** Initializes a BankerParse from a PolytreeParse.
    *
    * @param parse the parse to initialize from
    * @return the analogous BankerParse
    */
  def fromPolytreeParse(parse: PolytreeParse): BankerParse = {
    BankerParse(parse.sentence, parse.breadcrumb, parse.children,
      parse.arclabels map { labelset =>
        labelset map {
          case (index, label) =>
            (index, label.toSymbol)
        }
      })
  }
}
