package org.allenai.nlpstack.parse.poly.polyparser

import spray.json.DefaultJsonProtocol._

/** Collects statistics over "neighborhood events."
  *
  * An example might help. A neighborhood is a collection of tokens, e.g. a node and its children
  * in a dependency parse. A neighborhood event is a mapping of these tokens to a sequence of
  * strings, e.g. we might map each token to its part-of-speech tag.
  *
  * Given a corpus of dependency parses, we might want to collect a histogram that tells us
  * how many times each neighborhood event like (VERB, NOUN, NOUN) occurs in the corpus.
  * This is what the NeighborhoodEventStatistic does.
  *
  * @param name a label for this object
  * @param neighborhoodCounts a histogram over observed neighborhoods
  * @param eventTransform a transformation from neighborhoods to events
  */
case class NeighborhoodEventStatistic(name: String, neighborhoodCounts: Seq[(Neighborhood, Int)],
  eventTransform: NeighborhoodTransform) {

  /** Computes a smoothed probability estimate of the given neighborhood's event. */
  def getSmoothedEventProbability(neighborhood: Neighborhood): Double = {
    (getNeighborhoodCount(neighborhood) + 0.5) /
      (transformedEventCountSum + 0.5)
  }

  private def getNeighborhoodCount(neighborhood: Neighborhood): Int = {
    transformedEventCounts.getOrElse(eventTransform(neighborhood), 0)
  }

  @transient private val transformedEventCounts: Map[Seq[String], Int] = {
    var sigMap = Map[Seq[String], Int]()
    for {
      (neighborhood, count) <- neighborhoodCounts
      transformedEvent = eventTransform(neighborhood)
    } {
      sigMap = sigMap + (transformedEvent -> (count + sigMap.getOrElse(transformedEvent, 0)))
    }
    sigMap
  }

  @transient private val transformedEventCountSum: Int = {
    transformedEventCounts.values reduce { (x, y) => x + y }
  }

  override def toString(): String = {
    (transformedEventCounts.toSeq.sortBy { _._2 } map { case (event, count) =>
      s"${count}: $event"
    }).mkString("\n") + s"\nTotal observations: ${transformedEventCountSum}"
  }
}

object NeighborhoodEventStatistic {
  implicit val eventStatisticJsonFormat = jsonFormat3(NeighborhoodEventStatistic.apply)
}

