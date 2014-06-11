package org.allenai.nlpstack.srl

import org.allenai.nlpstack.LineProcessor
import org.allenai.nlpstack.parse.DependencyParser
import org.allenai.nlpstack.parse.graph._
import org.allenai.nlpstack.postag.PostaggedToken

abstract class SrlMain extends LineProcessor("srl") {
  def srl: Srl

  override def process(line: String) = {
    val (tokens, dgraph) = DependencyParser.multilineStringFormat.read(line)
    (srl(tokens, dgraph) map (_.serialize)).mkString("\n")
  }
}
