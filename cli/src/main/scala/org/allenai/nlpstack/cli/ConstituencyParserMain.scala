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
