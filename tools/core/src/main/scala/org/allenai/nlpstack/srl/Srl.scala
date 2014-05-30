package org.allenai.nlpstack
package srl

import org.allenai.nlpstack.LineProcessor
import org.allenai.nlpstack.parse.DependencyParser
import org.allenai.nlpstack.parse.graph.DependencyGraph
import scala.concurrent.Await
import scala.concurrent.duration._
import org.allenai.nlpstack.postag.PostaggedToken

abstract class Srl {
  def apply(tokens: Seq[PostaggedToken], graph: DependencyGraph): Seq[Frame]
}
