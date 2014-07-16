package org.allenai.nlpstack.webapp.tools

import org.allenai.nlpstack.chunk.OpenNlpChunker
import org.allenai.nlpstack.lemmatize.MorphaStemmer
import org.allenai.nlpstack.parse.PolytreeParser
import org.allenai.nlpstack.postag.defaultPostagger
import org.allenai.nlpstack.segment.defaultSegmenter
import org.allenai.nlpstack.tokenize.defaultTokenizer

object Impl {
  private[tools] val sentenceSegmenter = defaultSegmenter
  private[tools] val tokenizer = defaultTokenizer
  private[tools] val lemmatizer = new MorphaStemmer()
  private[tools] lazy val postagger = defaultPostagger
  private[tools] val chunker = new OpenNlpChunker()
  private[tools] lazy val dependencyParser = new PolytreeParser

  val obamaText = "Barack Hussein Obama II is the 44th and current President of the United States, and the first African American to hold the office. Born in Honolulu, Hawaii, Obama is a graduate of Columbia University and Harvard Law School, where he served as president of the Harvard Law Review. He was a community organizer in Chicago before earning his law degree. He worked as a civil rights attorney and taught constitutional law at the University of Chicago Law School from 1992 to 2004. He served three terms representing the 13th District in the Illinois Senate from 1997 to 2004, running unsuccessfully for the United States House of Representatives in 2000."
  val obamaSentences = sentenceSegmenter(obamaText) map (_.text) mkString "\n"
}