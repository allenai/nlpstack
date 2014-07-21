package org.allenai.nlpstack.coref

import org.allenai.nlpstack.core.Token
import org.allenai.nlpstack.core.coref._
import org.allenai.nlpstack.core.parse.graph.DependencyGraph

class FactorieCorefResolver[T <: Token] extends CorefResolver[T] {
  def resolveCoreferences(postaggedParse: (Seq[T], DependencyGraph)) = {
    Seq()
  }

}
