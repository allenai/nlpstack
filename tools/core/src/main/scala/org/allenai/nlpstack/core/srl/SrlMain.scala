package org.allenai.nlpstack.core.srl

import org.allenai.nlpstack.core.LineProcessor
import org.allenai.nlpstack.core.parse.DependencyParser
import org.allenai.nlpstack.core.parse.graph._
import org.allenai.nlpstack.core.postag.PostaggedToken

abstract class SrlMain extends LineProcessor("srl") {
  def srl: Srl

  override def process(line: String) = {
    val (tokens, dgraph) = DependencyParser.multilineStringFormat.read(line)
    (srl(tokens, dgraph) map (_.serialize)).mkString("\n")
  }
}
