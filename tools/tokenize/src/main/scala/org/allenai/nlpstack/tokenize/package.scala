package org.allenai.nlpstack

package object tokenize {
  @deprecated("Please use defaultTokenizer instead", "2014-06-19")
  type SimpleEnglishTokenizer = FactorieTokenizer

  def defaultTokenizer: Tokenizer = new FactorieTokenizer
}
