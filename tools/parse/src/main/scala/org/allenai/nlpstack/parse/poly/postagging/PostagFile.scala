package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.parse.poly.polyparser.ConllX
import org.allenai.nlpstack.postag.StanfordPostagger
import scopt.OptionParser
import org.allenai.nlpstack.postag.defaultPostagger

private case class PostagFileCommandLine(configFilename: String = "", testFilename: String = "",
  dataSource: String = "")

object PostagFile {

  def main(args: Array[String]) {
    val optionParser = new OptionParser[PostagFileCommandLine]("PostagFile") {
      opt[String]('c', "config") required () valueName ("<file>") action { (x, c) => c.copy(configFilename = x) } text ("the file containing the JSON " +
        " configuration for the parser")
      opt[String]('t', "test") required () valueName ("<file>") action { (x, c) => c.copy(testFilename = x) } text ("the file containing the test " +
        "parses to parse and compare against (in ConllX format, comma-separated filenames)")
      opt[String]('d', "datasource") required () valueName ("<file>") action { (x, c) => c.copy(dataSource = x) } text ("the location of the data " +
        "('datastore','local')") validate { x =>
          if (Set("datastore", "local").contains(x)) {
            success
          } else {
            failure(s"unsupported data source: ${x}")
          }
        }
    }
    val config: PostagFileCommandLine = optionParser.parse(args, PostagFileCommandLine()).get

    println("Evaluating Stanford tagger:")
    val stanTagger: PolyPostagger = NLPStackPostagger(new StanfordPostagger())
    SimplePostagger.fullTaggingEvaluation(stanTagger, config.testFilename, ConllX(true), config.dataSource, 0)

    println("Evaluating Factorie tagger:")
    val factorieTagger: PolyPostagger = NLPStackPostagger(defaultPostagger)
    SimplePostagger.fullTaggingEvaluation(factorieTagger, config.testFilename, ConllX(true), config.dataSource, 0)

    println("Evaluating serialized tagger:")
    val tagger: SimplePostagger = SimplePostagger.load(config.configFilename)
    SimplePostagger.fullTaggingEvaluation(tagger, config.testFilename, ConllX(true), config.dataSource, 0)
  }
}

