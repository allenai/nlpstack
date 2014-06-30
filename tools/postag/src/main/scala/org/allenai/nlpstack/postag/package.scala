package org.allenai.nlpstack

package object postag {
  val defaultPostagger: Postagger = new FactoriePostagger
}
