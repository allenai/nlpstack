package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.nlpstack.parse.poly.ml.BrownClusters
import scopt.OptionParser
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

  @transient val uniqueEventCount = transformedEventCounts.size

  @transient val singletonEventCount =
    (transformedEventCounts filter { case (_, eventCount) => eventCount == 1 }).size

  @transient private val transformedEventCountSum: Int = {
    transformedEventCounts.values reduce { (x, y) => x + y }
  }

  override def toString(): String = {
    (transformedEventCounts.toSeq.sortBy { _._2 } map {
      case (event, count) =>
        s"${count}: $event"
    }).mkString("\n") + s"\nTotal observations: ${transformedEventCountSum}"
  }
}

object NeighborhoodEventStatistic {
  implicit val eventStatisticJsonFormat = jsonFormat3(NeighborhoodEventStatistic.apply)

  def main(args: Array[String]) {
    val optionParser = new OptionParser[PRTPOCommandLine]("ParseRerankerTrainingPhaseOne") {
      opt[String]('g', "goldfile") required () valueName ("<file>") action { (x, c) => c.copy(goldParseFilename = x) } text ("the file containing the gold parses")
      opt[String]('c', "clusters") valueName ("<file>") action { (x, c) => c.copy(clustersPath = x) } text ("the path to the Brown cluster files " +
        "(in Liang format, comma-separated filenames)")
      opt[String]('d', "datasource") required () valueName ("<file>") action { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
        "('datastore','local')") validate { x =>
          if (Set("datastore", "local").contains(x)) {
            success
          } else {
            failure(s"unsupported data source: ${x}")
          }
        }
    }
    val clArgs: PRTPOCommandLine =
      optionParser.parse(args, PRTPOCommandLine()).get
    val clusters: Seq[BrownClusters] = {
      if (clArgs.clustersPath != "") {
        clArgs.clustersPath.split(",") map { path =>
          BrownClusters.fromLiangFormat(path)
        }
      } else {
        Seq[BrownClusters]()
      }
    }

    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )

    println("Initializing features.")
    //val leftChildNeighborhoodCounts = ("leftChild", LeftChildrenExtractor,
    //  Neighborhood.countNeighborhoods(
    //    new ExtractorBasedNeighborhoodSource(goldParseSource, LeftChildrenExtractor)))
    //val rightChildNeighborhoodCounts = ("rightChild", RightChildrenExtractor,
    //  Neighborhood.countNeighborhoods(
    //    new ExtractorBasedNeighborhoodSource(goldParseSource, RightChildrenExtractor)))
    /*
    val childNeighborhoodCounts = ("child",
      ChildrenExtractor,
      Neighborhood.countNeighborhoods(
        new ExtractorBasedNeighborhoodSource(goldParseSource, ChildrenExtractor)
      ))

    val crumbNeighborhoodCounts = ("crumb",
      BreadcrumbExtractor,
      Neighborhood.countNeighborhoods(
        new ExtractorBasedNeighborhoodSource(goldParseSource, BreadcrumbExtractor)
      ))

    //val path3NeighborhoodCounts = ("path3", RootPathExtractor(3),
    //  Neighborhood.countNeighborhoods(
    //    new ExtractorBasedNeighborhoodSource(goldParseSource, RootPathExtractor(3))))

    val transforms = Seq(
      ("brown", BrownTransform(clusters.head, 100, "brown")),
      ("arclabel", TokenPropTransform('arclabel)),
      ("cpos", TokenPropTransform('cpos)),
      ("pos", TokenPropTransform('factoriePos))
    )

    val stat = NeighborhoodEventStatistic("arclabelstat", childNeighborhoodCounts._3,
      TokenPropTransform('cpos))
    println(stat.toString)
    println(s"Number of unique events: ${stat.uniqueEventCount}")
    println(s"Number of singleton events: ${stat.singletonEventCount}")
    */
  }
}

