package org.allenai.nlpstack.core.coref

import org.allenai.nlpstack.core.Token
import org.allenai.nlpstack.core.parse.graph.{ DependencyNode, DependencyGraph }

case class Referent(
  val references: Seq[DependencyNode],
  val mainReference: Option[DependencyNode])

abstract class CorefResolver[T <: Token] {
  def resolveCoreferences(postaggedParse: (Seq[T], DependencyGraph)): Seq[Referent]
}
