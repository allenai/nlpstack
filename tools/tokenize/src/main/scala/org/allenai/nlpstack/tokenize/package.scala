package org.allenai.nlpstack

import org.allenai.nlpstack.core.Tokenizer

package object tokenize {
  def defaultTokenizer: Tokenizer = StanfordTokenizer
}
