package org.allenai.nlpstack.parse.poly.fsm

import org.allenai.nlpstack.parse.poly.core.Util
import org.allenai.nlpstack.parse.poly.ml.{ CandidatePool, CandidatePoolCorpus }
import spray.json.DefaultJsonProtocol._
import spray.json._

/** A sequence of (scored) sculptures. */
case class NbestList(scoredSculptures: Iterable[(Sculpture, Double)]) {

  /** Converts this n-best list into a candidate pool.
    *
    * Each sculpture is converted to a feature vector using the supplied feature, and
    * is scored using the supplied cost function.
    *
    * @param feature used to convert each sculpture to a feature vector
    * @param costFunction used to attach a cost to each feature vector
    * @return the resulting candidate pool
    */
  def toCandidatePool(feature: SculptureFeature, costFunction: SculptureCost): CandidatePool = {
    CandidatePool((scoredSculptures map {
      case (sculpture, baseScore) =>
        (feature(sculpture), costFunction(sculpture))
    }).toMap)
  }
}

object NbestList {
  implicit val jsFormat = jsonFormat1(NbestList.apply)
}

/** A sequence of NbestLists. */
case class NbestCorpus(nbestLists: Iterable[NbestList]) {

  /** Runs .toCandidatePool on each NbestList of this corpus.
    *
    * @param feature used to convert each sculpture to a feature vector
    * @param costFunction used to attach a cost to each feature vector
    * @return the resulting candidate pool corpus
    */
  def toCandidatePoolCorpus(feature: SculptureFeature,
    costFunction: SculptureCost): CandidatePoolCorpus = {

    CandidatePoolCorpus(nbestLists map { nbestList =>
      nbestList.toCandidatePool(feature, costFunction)
    })
  }
}

object NbestCorpus {
  implicit val jsFormat = jsonFormat1(NbestCorpus.apply)

  def loadNbestCorpus(filename: String): NbestCorpus = {
    val jsValue = Util.getJsValueFromFile(filename)
    jsValue match {
      case JsObject(values) =>
      case _ => deserializationError("Unexpected JsValue type. Must be " +
        "JsObject.")
    }
    jsValue.convertTo[NbestCorpus]
  }
}
