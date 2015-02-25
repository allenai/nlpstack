package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.ml.FeatureName
import org.allenai.nlpstack.parse.poly.polyparser.{ ConllX, InMemoryPolytreeParseSource }
import scopt.OptionParser

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
  * @param neighborhoodSource provides a stream of neighborhoods
  * @param eventTransform a transformation from neighborhoods to events
  */
case class NeighborhoodEventStatistic(
    neighborhoodSource: NeighborhoodSource,
    eventTransform: NeighborhoodTransform
) {

  /** Maps each neighborhood event to the count of its observations in the event stream. */
  @transient private val eventCounts: Map[FeatureName, Int] = {
    var sigMap = Map[FeatureName, Int]()
    for {
      (parse, neighborhood) <- neighborhoodSource.getNeighborhoodIterator()
      event <- eventTransform(parse, neighborhood)
    } {
      sigMap = sigMap + (event -> (1 + sigMap.getOrElse(event, 0)))
    }
    sigMap
  }

  /** The total number of observed (non-unique) neighborhood events. */
  @transient val observedEventCount: Int =
    eventCounts.values reduce { (x, y) => x + y }

  /** The total number of unique neighborhood events. */
  @transient val uniqueEventCount = eventCounts.size

  /** The total number of singleton (observed only once) neighborhood events. */
  @transient val singletonEventCount =
    (eventCounts filter { case (_, eventCount) => eventCount == 1 }).size

  override def toString(): String = {
    (eventCounts.toSeq.sortBy { case (_, count) => count } map {
      case (event, count) =>
        s"${count}: $event"
    }).mkString("\n") + s"\nTotal observations: ${observedEventCount}"
  }
}

private case class NESCommandLine(goldParseFilename: String = "", dataSource: String = "")

object NeighborhoodEventStatistic {

  /** A toy command-line to extract neighborhood event statistics from a set of gold parses.
    *
    * format: OFF
    * Usage: NeighborhoodEventStatistic [options]
    *
    *   -g <file> | --goldfile <file>
    *         the file containing the gold parses
    *   -d <file> | --datasource <file>
    *         the location of the data ('datastore','local')
    * format: ON
    *
    * @param args see above
    */
  def main(args: Array[String]) {
    val optionParser = new OptionParser[NESCommandLine]("NeighborhoodEventStatistic") {
      opt[String]('g', "goldfile") required () valueName "<file>" action { (x, c) =>
        c.copy(goldParseFilename = x) } text "the file containing the gold parses"
      opt[String]('d', "datasource") required () valueName "<file>" action { (x, c) =>
        c.copy(dataSource = x) } text ("the location of the data " +
        "('datastore','local')") validate { x =>
          if (Set("datastore", "local").contains(x)) {
            success
          } else {
            failure(s"unsupported data source: $x")
          }
        }
    }
    val clArgs: NESCommandLine =
      optionParser.parse(args, NESCommandLine()).get
    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val stat = NeighborhoodEventStatistic(
      new ExtractorBasedNeighborhoodSource(goldParseSource, AllChildrenExtractor),
      PropertyNhTransform('cpos)
    )
    println(stat.toString)
    println(s"Number of unique events: ${stat.uniqueEventCount}")
    println(s"Number of singleton events: ${stat.singletonEventCount}")
  }
}
