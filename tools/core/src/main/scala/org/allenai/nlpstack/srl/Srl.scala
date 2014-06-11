package org.allenai.nlpstack.srl

import org.allenai.nlpstack.LineProcessor
import org.allenai.nlpstack.parse.DependencyParser
import org.allenai.nlpstack.parse.graph.DependencyGraph
import org.allenai.nlpstack.postag.PostaggedToken

import scala.concurrent.Await
import scala.concurrent.duration._

abstract class Srl {
  def apply(tokens: Seq[PostaggedToken], graph: DependencyGraph): Seq[Frame]
}
