package org.allenai.nlpstack

import org.allenai.nlpstack.core.tokenize.Tokenizer

package object tokenize {
  def defaultTokenizer: Tokenizer = new FactorieTokenizer
}
