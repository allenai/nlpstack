package org.allenai.nlpstack

package object tokenize {
  // Other places depend on SimpleEnglishTokenizer to exist, so for now we give
  // it to them this way.
  type SimpleEnglishTokenizer = FactorieTokenizer
}
