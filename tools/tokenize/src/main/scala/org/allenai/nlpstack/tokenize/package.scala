package org.allenai.nlpstack

package object tokenize {
  def defaultTokenizer: Tokenizer = new FactorieTokenizer
}
