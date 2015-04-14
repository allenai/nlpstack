package org.allenai.nlpstack.parse.poly.reranking

import org.allenai.nlpstack.parse.poly.core.Sentence
import org.allenai.nlpstack.parse.poly.decisiontree.{
  DecisionTreeJustification,
  Justification,
  RandomForestJustification
}
import org.allenai.nlpstack.parse.poly.eval._
import org.allenai.nlpstack.parse.poly.fsm.RerankingFunction
import org.allenai.nlpstack.parse.poly.ml.{
  FeatureName,
  JustifyingWrapperClassifier,
  WrapperClassifier
}
import org.allenai.nlpstack.parse.poly.polyparser._

import java.io._

import scopt.OptionParser

case class ParseRerankerCommandLine(
  parserFilename: String = "",
  goldParseFilename: String = "",
  dataSource: String = "",
  rerankerFilename: String = "",
  diagnosticFilename: String = ""
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
      opt[String]('l', "diagnosticfile") required () valueName "<string>" action { (x, c) =>
        c.copy(diagnosticFilename = x)
      } text "where to write the reranking function"
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

    val diagnosticWriter = new PrintWriter(new File(clArgs.diagnosticFilename))

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
        ParseEvaluator.evaluate(
          candidateParses, goldParseSource.parseIterator, stats, Some(diagnosticWriter)
        )
    }
    diagnosticWriter.close
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
      val notWeirdCandidateNodes = rerankingFunction.getNotWeirdNodes(candParse)
      val badCandidateTokens: Set[Int] =
        (candParse.families.toSet -- goldParse.families.toSet) map {
          case family =>
            family.tokens.head
        } filter { tokIndex =>
          candParse.tokens(tokIndex).getDeterministicProperty('cpos) != Symbol(".")
        }

      val falsePositiveMessages = weirdGoldNodes map {
        case (goldNodeIx, justificationOption) =>
          s"  ${goldParse.printFamily(goldNodeIx)}" +
            s"\nClassifier Explanation: ${prettyPrintWeirdnessExplanation(justificationOption)}\n"
      }
      val trueNegativeNodesAndJustifications = notWeirdCandidateNodes filter {
        node => badCandidateTokens.contains(node._1)
      }
      val trueNegativeMessages = trueNegativeNodesAndJustifications map {
        case (misclassifiedNodeIx, justificationOption) =>
          s"  ${candParse.printFamily(misclassifiedNodeIx)}\n" +
            s"    (should be: ${goldParse.printFamily(misclassifiedNodeIx)})" +
            s" \nClassifier Explanation: ${prettyPrintWeirdnessExplanation(justificationOption)}\n"
      }

      if (falsePositiveMessages.nonEmpty || trueNegativeMessages.nonEmpty) {
        sentencesWithMistakes = sentencesWithMistakes :+
          Tuple3(goldParse.sentence, falsePositiveMessages, trueNegativeMessages)
      }
    }
  }

  override def report(diagnosticWriter: Option[PrintWriter]): Unit = {
    sentencesWithMistakes foreach {
      case (sentence, falsePositiveMessages, trueNegativeMessages) =>
        logMessage(diagnosticWriter, "")
        logMessage(diagnosticWriter, sentence.asWhitespaceSeparatedString)
        if (falsePositiveMessages.nonEmpty) {
          logMessage(diagnosticWriter, "Good families, classified as weird:")
          falsePositiveMessages foreach { message =>
            logMessage(diagnosticWriter, message)
          }
        }
        if (trueNegativeMessages.nonEmpty) {
          logMessage(diagnosticWriter, "Bad families, classified as good:")
          trueNegativeMessages foreach { message =>
            logMessage(diagnosticWriter, message)
          }
        }
    }
  }

  override def reset(): Unit = {
    sentencesWithMistakes = Seq[(Sentence, Set[String], Set[String])]()
  }

  /** Helper method used to check whether the WrapperClassifier in use is a
    * JustifyingWrapperClassifier.
    */
  private def getJustifyingClassifier(
    classifier: WrapperClassifier
  ): Option[JustifyingWrapperClassifier] = {
    classifier match {
      case justifyingClassifier: JustifyingWrapperClassifier => Some(justifyingClassifier)
      case _ => None
    }
  }

  /** Helper Method used to custom print a DecisionTreeJustification for Weirdness analysis.
    *
    * For a Justification that looks like:
    * [ parent1.cpos.nexus = 0, child.cpos.. = 0, parent2.cpos.. = 0, self.cpos.notFound = 0,
    * parent2.alabel.ADVMOD = 0, child1.direction.R = 0, self.cpos.VERB = 0,
    * parent.cpos.notFound = 0, children.card.0 = 0, children.card.1 = 1, parent.suffix.s = 0,
    * parent.cpos.ADP = 0, self.keyword.'s = 0, child1.alabel.AMOD = 0, child1.alabel.DEP = 0,
    * child1.alabel.NSUBJ = 0, parent.cpos.. = 0, child.keyword.was = 0, child1.alabel.TMOD = 0,
    * child1.alabel.COP = 0, child1.alabel.POSSESSIVE = 0, child1.keyword.is = 0,
    * child1.alabel.PRT = 0, self.cpos.CONJ = 0, child1.alabel.VMOD = 0, child.cpos.NOUN = 0,
    * self.cpos.ADP = 1, child1.keyword.did = 0, child.keyword.are = 0, self.keyword.at = 1 ]
    *
    * Explanation generated will look like:
    * [
    * [ children.card = 1 ],
    * [ self.cpos = ADP ],
    * [ self.keyword = at ],
    * [ child.cpos <> {NOUN, .} ],
    * [ child.keyword <> {are, was} ],
    * [ child1.alabel <> {VMOD, AMOD, NSUBJ, COP, DEP, POSSESSIVE, TMOD, PRT} ],
    * [ child1.direction <> R ],
    * [ child1.keyword <> {did, is} ],
    * [ parent.cpos <> {., notFound, ADP} ],
    * [ parent.suffix <> s ],
    * [ parent1.cpos <> nexus ],
    * [ parent2.alabel <> ADVMOD ],
    * [ parent2.cpos <> . ]
    * ]
    */
  private def prettyPrintDecisionTreeWeirdnessExplanation(
    dtJustification: DecisionTreeJustification,
    justifyingClassifier: JustifyingWrapperClassifier
  ): String = {
    val explainableJustification =
      justifyingClassifier.getExplainableDecisionTreeJustification(dtJustification)
    // Group the tuples by the first + second symbol,  which indicate the node in the polytree,
    // and the property in question, respectively,for e.g.,
    // "self.cpos", "parent1.direction", etc.
    val featureValuesGrouped =
      (explainableJustification.groupBy(_._1.symbols.take(2)) map {
        case (k, v) => (new FeatureName(k), v)
      }) mapValues {
        case featureValueMap: Map[FeatureName, Int] =>
          featureValueMap map { t => (new FeatureName(t._1.symbols.drop(2)).toString, t._2) }
      }

    // Helper Method to format output feature values.
    def setBeginMarker(featureVals: Seq[(String, Int)]): String = {
      if (featureVals.size > 1) "{"
      else ""
    }

    // Helper Method to format output feature values.
    def setEndMarker(featureVals: Seq[(String, Int)]): String = {
      if (featureVals.size > 1) "}"
      else ""
    }

    // Build the final explanation (string) by composing justification for each type of node.
    // For each key- first + second symbol of the FeatureName, for e.g., "self" and "cpos",
    // in the feature map, get the various binary property values, for e.g., "ADJ=0", "ADV =1", etc.
    // Group map elements by the true/false property values for a more meaningful display.
    // Within each group (true/false), sort the keys alphabetically, for e.g., "parent1.direction"
    // should appear before "self.cpos".
    val explanationsGroupedByTrueFalseProperty = (for {
      (k, v) <- featureValuesGrouped
    } yield {
      val featureNameStr = k.toString
      // If there exists any property in the map that has a value of '1' (is true), then show
      // only that property as true. If not, construct a friendly representation for
      // all off properties.
      val vSeq = v.toSeq
      val featureValStrAndTrueFalseProperty = vSeq.find(x => x._2 == 1) match {
        case Some(trueProperty) => (" = " + trueProperty._1, true)
        case _ => (" <> " + setBeginMarker(vSeq) + vSeq.map(x => x._1).mkString(", ") +
          setEndMarker(vSeq), false)
      }
      (featureNameStr, featureValStrAndTrueFalseProperty._1, featureValStrAndTrueFalseProperty._2)
    }).groupBy(x => x._3).mapValues(v => v.toSeq.sortBy(_._1).map(y => (y._1, y._2)))

    val trueFeatureValueExplanations = for {
      truePropertyExplanation <- explanationsGroupedByTrueFalseProperty.getOrElse(
        true, Seq.empty[(String, String)]
      )
    } yield {
      "  [ " + truePropertyExplanation._1 + truePropertyExplanation._2 + " ]"
    }
    val falseFeatureValueExplanations = for {
      falsePropertyExplanation <- explanationsGroupedByTrueFalseProperty.getOrElse(false, Seq.empty[(String, String)])
    } yield {
      "  [ " + falsePropertyExplanation._1 + falsePropertyExplanation._2 + " ]"
    }

    "\n[\n" +
      (trueFeatureValueExplanations ++ falseFeatureValueExplanations).mkString(",\n") +
      "\n]"
  }

  /** Method used to custom print a random forest's justification for Weirdness
    * analysis.
    */
  def prettyPrintWeirdnessExplanation(justificationOption: Option[Justification]): String = {
    (for {
      justifyingClassifier <- getJustifyingClassifier(rerankingFunction.classifier)
      justification <- justificationOption
    } yield {
      justification match {
        case dtJustification: DecisionTreeJustification =>
          prettyPrintDecisionTreeWeirdnessExplanation(dtJustification, justifyingClassifier)
        case rfJustification: RandomForestJustification =>
          "[ " + rfJustification.dtJustifications.map(dtJustification =>
            prettyPrintDecisionTreeWeirdnessExplanation(dtJustification, justifyingClassifier)).
            mkString(", ") + " ]"
      }
    }).getOrElse("")
  }

}
