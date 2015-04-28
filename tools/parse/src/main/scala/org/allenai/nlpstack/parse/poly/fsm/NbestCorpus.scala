package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.core.Util

import reming.DefaultJsonProtocol._

/** A sequence of (scored) sculptures. */
case class NbestList(scoredSculptures: Iterable[(Sculpture, Double)])

object NbestList {
  implicit val jsFormat = jsonFormat1(NbestList.apply)
}

/** A sequence of NbestLists. */

case class NbestCorpus(nbestLists: Iterable[NbestList])

object NbestCorpus {
  implicit val jsFormat = jsonFormat1(NbestCorpus.apply)

  def loadNbestCorpus(filename: String): NbestCorpus = Util.readFromFile(filename)
}
