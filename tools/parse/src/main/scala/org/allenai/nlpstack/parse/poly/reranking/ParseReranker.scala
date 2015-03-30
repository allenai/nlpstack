package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.core.Sentence
import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.fsm.RerankingFunction
import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

case class ParseRerankerCommandLine(
  parserFilename: String = "",
  goldParseFilename: String = "",
  dataSource: String = "",
  rerankerFilename: String = ""
)

/** This command-line takes a serialized reranking function and uses it to rerank an n-best
  * list. The resulting parses are then evaluated against a gold set.
  */
object ParseReranker {

  def main(args: Array[String]) {
    val optionParser = new OptionParser[ParseRerankerCommandLine]("ParseRerankerTraining") {
      opt[String]('p', "parser") required () valueName "<file>" action { (x, c) =>
        c.copy(parserFilename = x)
      } text "the file containing the nbest lists"
      opt[String]('g', "goldfile") required () valueName "<file>" action { (x, c) =>
        c.copy(goldParseFilename = x)
      } text "the file containing the gold parses"
      opt[String]('r', "reranker") required () valueName "<file>" action { (x, c) =>
        c.copy(rerankerFilename = x)
      } text "the file containing the serialized reranking function"
      opt[String]('d', "datasource") required () valueName "<file>" action { (x, c) =>
        c.copy(dataSource = x)
      } text "the location of the data ('datastore','local')" validate { x =>
        if (Set("datastore", "local").contains(x)) {
          success
        } else {
          failure(s"unsupported data source: $x")
        }
      }
    }
    val clArgs: ParseRerankerCommandLine =
      optionParser.parse(args, ParseRerankerCommandLine()).get
    val goldParseSource = InMemoryPolytreeParseSource.getParseSource(
      clArgs.goldParseFilename,
      ConllX(true, makePoly = true), clArgs.dataSource
    )
    val parser: TransitionParser = TransitionParser.load(clArgs.parserFilename)
    val reranker = RerankingFunction.load(clArgs.rerankerFilename)

    println("Parsing test set.")
    val candidateParses: Iterator[Option[PolytreeParse]] = {
      goldParseSource.parseIterator map {
        parse => parser.parse(parse.sentence)
      }
    }
    reranker match {
      case weirdReranker: WeirdParseNodeRerankingFunction =>
        val stats: Seq[ParseStatistic] = Seq(WeirdnessAnalyzer(weirdReranker))
        stats foreach { stat => stat.reset() }
        ParseEvaluator.evaluate(candidateParses, goldParseSource.parseIterator, stats)
    }
  }
}

/** A parse statistic that collects weirdness statistics for candidate parses.
  *
  * @param rerankingFunction the weirdness reranking function
  */
case class WeirdnessAnalyzer(rerankingFunction: WeirdParseNodeRerankingFunction)
    extends ParseStatistic {

  var sentencesWithMistakes = Seq[(Sentence, Set[String], Set[String])]()

  override def notify(candidateParse: Option[PolytreeParse], goldParse: PolytreeParse): Unit = {
    candidateParse map { candParse =>

      val weirdGoldNodes = rerankingFunction.getWeirdNodes(goldParse)
      val weirdCandidateNodes = rerankingFunction.getWeirdNodes(candParse)
      val badCandidateTokens: Set[Int] =
        (candParse.labeledFamilies.toSet -- goldParse.labeledFamilies.toSet) map {
          case (node, family) =>
            node
        } filter { tokIndex =>
          candParse.tokens(tokIndex).getDeterministicProperty('cpos) != Symbol(".")
        }

      val falsePositiveMessages = weirdGoldNodes map { goldNode =>
        s"  ${goldParse.printFamily(goldNode)}"
      }
      val trueNegativeMessages =
        (badCandidateTokens -- weirdCandidateNodes) map { misclassifiedNode =>
          s"  ${goldParse.printFamily(misclassifiedNode)}"
        }
      if (falsePositiveMessages.nonEmpty || trueNegativeMessages.nonEmpty) {
        sentencesWithMistakes = sentencesWithMistakes :+
          (goldParse.sentence, falsePositiveMessages, trueNegativeMessages)
      }
    }
  }

  override def report(): Unit = {
    sentencesWithMistakes foreach {
      case (sentence, falsePositiveMessages, trueNegativeMessages) =>
        println("")
        println(sentence.asWhitespaceSeparatedString)
        if (falsePositiveMessages.nonEmpty) {
          println("Good families, classified as weird:")
          falsePositiveMessages foreach { message =>
            println(message)
          }
        }
        if (trueNegativeMessages.nonEmpty) {
          println("Bad families, classified as good:")
          trueNegativeMessages foreach { message =>
            println(message)
          }
        }
    }
  }

  override def reset(): Unit = {
    sentencesWithMistakes = Seq[(Sentence, Set[String], Set[String])]()
  }
}
