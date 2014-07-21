package org.allenai.nlpstack

import org.allenai.nlpstack.core.Token
import org.allenai.nlpstack.core.coref.CorefResolver

package object coref {
  def defaultCorefResolver[T <: Token]: CorefResolver[T] = new FactorieCorefResolver[T]
}
