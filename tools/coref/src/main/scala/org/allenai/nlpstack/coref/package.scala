package org.allenai.nlpstack

import org.allenai.nlpstack.core.PostaggedToken
import org.allenai.nlpstack.core.coref.CorefResolver

package object coref {
  def defaultCorefResolver: CorefResolver[PostaggedToken] =
    new FactorieCorefResolver
}
