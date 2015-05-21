package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.nlpstack.core.{ PostaggedToken, Lemmatized, Postagger }
import org.allenai.nlpstack.lemmatize.MorphaStemmer
import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.fsm._
import org.allenai.nlpstack.postag._

/** Wrapper for a postagger from NLPStack.
  *
  * @param baseTagger the underlying NLPStack postagger
  * @param useCoarseTags set to true if you want the tagger to produce Google coarse POS tags
  */
case class NLPStackPostagger(baseTagger: Postagger, useCoarseTags: Boolean) extends SentenceTagger {

  override def tag(
    sentence: Sentence
  ): SentenceTagging = {

    val taggedTokens: Map[Int, PostaggedToken] =
      SentenceTransform.getPostaggedTokens(sentence, baseTagger)
    val tagMap: Map[Int, Set[TokenTag]] = taggedTokens mapValues { taggedToken =>
      Set(
        if (useCoarseTags) {
          TokenTag(
            'autoCpos,
            Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(taggedToken.postag, "X"))
          )
        } else {
          TokenTag('autoPos, Symbol(taggedToken.postag))
        }
      )
    }
    SentenceTagging(sentence, tagMap)
  }
}

/** The FactorieLemmatizer tags the tokens of an input sentence with their lemmas, according
  * to the Factorie lemmatizer.
  */
case object NLPStackLemmatizer extends SentenceTagger {

  override def tag(sentence: Sentence): SentenceTagging = {
    val taggedTokens = SentenceTransform.getPostaggedTokens(sentence, defaultPostagger)
    val lemmaMap: Map[Int, Set[TokenTag]] =
      taggedTokens mapValues { tagged =>
        val lemmatized = Lemmatized[PostaggedToken](tagged, MorphaStemmer.lemmatize(tagged.string, tagged.postag))
        Set(TokenTag('autoLemma, Symbol(lemmatized.lemma)))
      }
    SentenceTagging(sentence, lemmaMap)
  }
}
