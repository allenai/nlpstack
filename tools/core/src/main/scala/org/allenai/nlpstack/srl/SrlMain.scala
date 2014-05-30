package org.allenai.nlpstack
package srl

import org.allenai.nlpstack.parse.graph._
import org.allenai.nlpstack.postag.PostaggedToken
import org.allenai.nlpstack.parse.DependencyParser

abstract class SrlMain extends LineProcessor("srl") {
  def srl: Srl

  override def process(line: String) = {
    val (tokens, dgraph) = DependencyParser.multilineStringFormat.read(line)
    (srl(tokens, dgraph) map (_.serialize)).mkString("\n")
  }
}
