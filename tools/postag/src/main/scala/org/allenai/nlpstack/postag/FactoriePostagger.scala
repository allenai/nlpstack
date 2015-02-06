package org.allenai.nlpstack.postag

import org.allenai.nlpstack.core._
import org.allenai.nlpstack.postag.FactoriePostagger.factorieFormat
import org.allenai.nlpstack.tokenize.FactorieTokenizer
import org.allenai.datastore.Datastore

import cc.factorie.app.nlp.{ Document => FactorieDocument }
import cc.factorie.app.nlp.pos.OntonotesForwardPosTagger
import cc.factorie.app.nlp.pos.PennPosTag

/** This is thread-safe. The only thing we call on OntonotesForwardPosTagger is
  * predict(), and predict() is threadsafe. I don't know about the other methods
  * on OntonotesForwardPosTagger.
  */
class FactoriePostagger extends Postagger {
  val tagger = FactoriePostagger.tagger

  override def postagTokenized(tokens: Seq[Token]): Seq[PostaggedToken] = {
    val factorieDoc = FactorieTokenizer.factorieFormat.write(tokens)
    val factorieTokens = factorieDoc.tokens.toSeq

    tagger.predict(factorieTokens) // modifies factorieTokens

    factorieFormat.read(factorieDoc)
  }
}

object FactoriePostagger {
  private val tagger =
    new OntonotesForwardPosTagger(
      Datastore.filePath(
      "cc.factorie.app.nlp.pos",
      "OntonotesForwardPosTagger.factorie",
      1
    ).toUri.toURL
    )

  object factorieFormat extends Format[Seq[PostaggedToken], FactorieDocument] {
    override def read(from: FactorieDocument): Seq[PostaggedToken] =
      from.tokens.map(t => PostaggedToken(
        tagger.tokenAnnotationString(t),
        t.string,
        t.stringStart
      )).toSeq

    override def write(from: Seq[PostaggedToken]): FactorieDocument = {
      val factorieDoc = FactorieTokenizer.factorieFormat.write(from)
      require(factorieDoc.tokenCount == from.size)
      (from, factorieDoc.tokens).zipped.foreach((token, factorieToken) => {
        factorieToken.attr += new PennPosTag(factorieToken, token.postag)
        factorieToken.attr += token
      })
      factorieDoc
    }
  }
}
