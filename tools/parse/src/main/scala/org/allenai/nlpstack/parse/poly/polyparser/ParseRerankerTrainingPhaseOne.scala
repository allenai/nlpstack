package org.allenai.nlpstack.parse.poly.polyparser

import java.io.{File, PrintWriter}

import org.allenai.nlpstack.parse.poly.fsm.RerankingFunction
import org.allenai.nlpstack.parse.poly.ml.{TrainingVectorPool, BrownClusters}
import scopt.OptionParser
import spray.json._


private case class PRTPOCommandLine(nbestFilenames: String = "",
  goldParseFilename: String = "", dataSource: String = "", clustersPath: String = "",
  vectorFilenames: String = "", rerankerFilename: String = "", otherGoldParseFilename: String = "")

object ParseRerankerTrainingPhaseOne {

  def main(args: Array[String]) {
    val optionParser = new OptionParser[PRTPOCommandLine]("ParseRerankerTrainingPhaseOne") {
      opt[String]('n', "nbestfiles") required () valueName ("<file>") action
        { (x, c) => c.copy(nbestFilenames = x) } text ("the file containing the nbest lists")
      opt[String]('g', "goldfile") required () valueName ("<file>") action
        { (x, c) => c.copy(goldParseFilename = x) } text ("the file containing the gold parses")
      opt[String]('h', "othergoldfile") required () valueName ("<file>") action
        { (x, c) => c.copy(otherGoldParseFilename = x) } text ("the file containing the other gold parses")
      opt[String]('c', "clusters") valueName ("<file>") action
        { (x, c) => c.copy(clustersPath = x) } text ("the path to the Brown cluster files " +
        "(in Liang format, comma-separated filenames)")
      opt[String]('o', "outputfile") required () valueName ("<file>") action
        { (x, c) => c.copy(rerankerFilename = x) } text ("where to write the incomplete" +
        " reranking function")
      opt[String]('v', "vectorfiles") required () valueName ("<file>") action
        { (x, c) => c.copy(vectorFilenames = x) } text ("where to write the training" +
        " vectors")
      opt[String]('d', "datasource") required () valueName ("<file>") action
        { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
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
      if(clArgs.clustersPath != "") {
        clArgs.clustersPath.split(",") map { path =>
          BrownClusters.fromLiangFormat(path)
        }
      } else {
        Seq[BrownClusters]()
      }
    }
    val inputSources: Seq[ParsePoolSource] =
      clArgs.nbestFilenames.split(",") map { path =>
        FileBasedParsePoolSource(path)
      }
    val vectorFiles: Seq[String] =
      clArgs.vectorFilenames.split(",")

    val otherGoldSource: PolytreeParseSource =
      MultiPolytreeParseSource(clArgs.otherGoldParseFilename.split(",") map { path =>
        InMemoryPolytreeParseSource.getParseSource(path,
          ConllX(true), clArgs.dataSource)
      })
    val parseCostFunction = OracleRerankingFunction(otherGoldSource.parseIterator)

    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource)


    println("Initializing features.")
    //val leftChildNeighborhoodCounts = ("leftChild", LeftChildrenExtractor,
    //  Neighborhood.countNeighborhoods(
    //    new ExtractorBasedNeighborhoodSource(goldParseSource, LeftChildrenExtractor)))
    //val rightChildNeighborhoodCounts = ("rightChild", RightChildrenExtractor,
    //  Neighborhood.countNeighborhoods(
    //    new ExtractorBasedNeighborhoodSource(goldParseSource, RightChildrenExtractor)))
    val childNeighborhoodCounts = ("child", ChildrenExtractor,
      Neighborhood.countNeighborhoods(
        new ExtractorBasedNeighborhoodSource(goldParseSource, ChildrenExtractor)))
    val crumbNeighborhoodCounts = ("crumb", BreadcrumbExtractor,
      Neighborhood.countNeighborhoods(
        new ExtractorBasedNeighborhoodSource(goldParseSource, BreadcrumbExtractor)))
    //val path3NeighborhoodCounts = ("path3", RootPathExtractor(3),
    //  Neighborhood.countNeighborhoods(
    //    new ExtractorBasedNeighborhoodSource(goldParseSource, RootPathExtractor(3))))

    val transforms = Seq(
      ("brown", BrownTransform(clusters.head, 100, "brown")),
      ("arclabel", TokenPropTransform('arclabel)),
      ("cpos", TokenPropTransform('cpos)),
      ("pos", TokenPropTransform('factoriePos))
    )

    val feature = new PolytreeParseFeatureUnion(Seq(
      SentenceLengthFeature,
      BaseParserScoreFeature,
      new EventStatisticFeatures(Seq(
        //leftChildNeighborhoodCounts,
        //rightChildNeighborhoodCounts,
        childNeighborhoodCounts,
        crumbNeighborhoodCounts
        //path3NeighborhoodCounts
      ), transforms)
    ))

    println("Saving incomplete reranker.")
    val incompleteRerankingFunction: RerankingFunction = LinearParseRerankingFunction(feature, None)
    val writer = new PrintWriter(new File(clArgs.rerankerFilename))
    try {
      writer.println(incompleteRerankingFunction.toJson)
    } finally {
      writer.close()
    }

    println("Creating training vectors.")
    inputSources zip vectorFiles foreach { case (inputSource, vectorFile) =>
      val scoredParsePools: Iterator[Iterable[((PolytreeParse, Double), Double)]] =
        for {
          parsePool <- inputSource.poolIterator
        } yield {
          for {
            (parse, origCost) <- parsePool.parses
          } yield {
            ((parse, origCost), parseCostFunction(parse))
          }
        }
      val trainingVectorPools: Iterator[TrainingVectorPool] = scoredParsePools map { pool =>
        createTrainingVectorPoolfromScoredParses(pool, feature)
      }
      TrainingVectorPool.writeTrainingVectorPools(trainingVectorPools, vectorFile)
    }
  }

  private def createTrainingVectorPoolfromScoredParses(
    scoredParses: Iterable[((PolytreeParse, Double), Double)],
    feature: PolytreeParseFeature): TrainingVectorPool = {

    TrainingVectorPool(
      scoredParses map { case ((parse, origCost), goldCost) =>
        (feature(parse, origCost), goldCost)
      }
    )
  }
}

