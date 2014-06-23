package org.allenai.nlpstack

package object postag {
  def defaultPostagger: Postagger = new FactoriePostagger
}
