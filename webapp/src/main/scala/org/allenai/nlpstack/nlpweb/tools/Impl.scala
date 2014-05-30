package org.allenai.nlpstack.nlpweb.tools

import org.allenai.nlpstack.segment.ChalkSentenceSegmenter
import org.allenai.nlpstack.tokenize.SimpleEnglishTokenizer
import org.allenai.nlpstack.lemmatize.MorphaStemmer
import org.allenai.nlpstack.postag.OpenNlpPostagger
import org.allenai.nlpstack.chunk.OpenNlpChunker
import org.allenai.nlpstack.parse.ClearParser

object Impl {
  private[tools] val sentenceSegmenter = new ChalkSentenceSegmenter()
  private[tools] val tokenizer = new SimpleEnglishTokenizer()
  private[tools] val lemmatizer = new MorphaStemmer()
  private[tools] val postagger = new OpenNlpPostagger()
  private[tools] val chunker = new OpenNlpChunker()
  private[tools] lazy val dependencyParser = new ClearParser()

  val obamaText = "Barack Hussein Obama II is the 44th and current President of the United States, and the first African American to hold the office. Born in Honolulu, Hawaii, Obama is a graduate of Columbia University and Harvard Law School, where he served as president of the Harvard Law Review. He was a community organizer in Chicago before earning his law degree. He worked as a civil rights attorney and taught constitutional law at the University of Chicago Law School from 1992 to 2004. He served three terms representing the 13th District in the Illinois Senate from 1997 to 2004, running unsuccessfully for the United States House of Representatives in 2000."
  val obamaSentences = sentenceSegmenter(obamaText) map (_.text) mkString "\n"
}