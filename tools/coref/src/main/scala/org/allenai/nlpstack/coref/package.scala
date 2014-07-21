package org.allenai.nlpstack

import org.allenai.nlpstack.core.coref.CorefResolver

package object coref {
  val defaultCorefResolver: CorefResolver = new FactorieCorefResolver
}
