package org.allenai.aitk
package srl

import org.allenai.aitk.LineProcessor
import org.allenai.aitk.parse.DependencyParser
import org.allenai.aitk.parse.graph.DependencyGraph
import scala.concurrent.Await
import scala.concurrent.duration._
import org.allenai.aitk.postag.PostaggedToken

abstract class Srl {
  def apply(tokens: Seq[PostaggedToken], graph: DependencyGraph): Seq[Frame]
}
