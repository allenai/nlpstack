package org.allenai.nlpstack.core.coref

import org.allenai.nlpstack.core.Token
import org.allenai.nlpstack.core.parse.graph.DependencyGraph

case class Referent[T <: Token](
  val references: Seq[T],
  val mainReference: Option[T])

abstract class CorefResolver[T <: Token] {
  def resolveCoreferences(postaggedParse: (Seq[T], DependencyGraph)) : Seq[Referent[T]]
}
