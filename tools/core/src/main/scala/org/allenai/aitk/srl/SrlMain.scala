package org.allenai.aitk
package srl

import org.allenai.aitk.parse.graph._
import org.allenai.aitk.postag.PostaggedToken
import org.allenai.aitk.parse.DependencyParser

abstract class SrlMain extends LineProcessor("srl") {
  def srl: Srl

  override def process(line: String) = {
    val (tokens, dgraph) = DependencyParser.multilineStringFormat.read(line)
    (srl(tokens, dgraph) map (_.serialize)).mkString("\n")
  }
}
