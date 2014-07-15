package org.allenai.nlpstack

import org.allenai.nlpstack.core.postag.Postagger

package object postag {
  val defaultPostagger: Postagger = new FactoriePostagger
}
