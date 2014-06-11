package org.allenai.nlpstack.parse

import org.allenai.nlpstack.LineProcessor

abstract class ConstituencyParserMain
    extends LineProcessor("constit-parser") {
  def constituencyParser: ConstituencyParser
  override def process(line: String) = {
    constituencyParser.parse(line).toString
  }
}
