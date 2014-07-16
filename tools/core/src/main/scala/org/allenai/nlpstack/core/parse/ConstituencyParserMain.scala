package org.allenai.nlpstack.core.parse

import org.allenai.nlpstack.core.LineProcessor

abstract class ConstituencyParserMain
    extends LineProcessor("constit-parser") {
  def constituencyParser: ConstituencyParser
  override def process(line: String) = {
    constituencyParser.parse(line).toString
  }
}
