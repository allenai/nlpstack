package org.allenai.nlpstack

import org.allenai.nlpstack.core.Postagger

package object postag {
  val defaultPostagger: Postagger = new FactoriePostagger
}
