package org.allenai.nlpstack.cli

import org.allenai.nlpstack.core.parse.ConstituencyParser
import org.allenai.nlpstack.parse.FactorieParser
import org.allenai.nlpstack.parse.PolytreeParser
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.tokenize.defaultTokenizer

abstract class ConstituencyParserMain
    extends LineProcessor("constit-parser") {
  def constituencyParser: ConstituencyParser
  override def process(line: String) = {
    constituencyParser.parse(line).toString
  }
}

object FactorieParserMain extends DependencyParserMain {
  override lazy val tokenizer = defaultTokenizer
  override lazy val postagger = defaultPostagger
  override lazy val dependencyParser = new FactorieParser
}

object PolytreeParserMain extends DependencyParserMain {
  override lazy val tokenizer = defaultTokenizer
  override lazy val postagger = defaultPostagger
  override lazy val dependencyParser = new PolytreeParser
}
