package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.polyparser.{ PolytreeParse }

import reming.DefaultJsonProtocol._

/** A Sculpture is a structured output corresponding to a final state of a finite-state
  * machine, whose goal is to transform an unstructured input (a MarbleBlock) into a
  * structured output.
  *
  * As an example, consider a transition-based parser. A MarbleBlock would be a sentence to be
  * parsed, whereas a Sculpture would be a parse tree for that sentence.
  */
trait Sculpture {
  def marbleBlock: MarbleBlock
}

object Sculpture {
  private implicit val polytreeParseFormat = jsonFormat4(PolytreeParse.apply)
  implicit val sculptureJsonFormat = parentFormat[Sculpture](childFormat[PolytreeParse, Sculpture])
}
