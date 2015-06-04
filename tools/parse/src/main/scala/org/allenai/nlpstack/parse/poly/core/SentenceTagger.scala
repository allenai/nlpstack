package org.allenai.nlpstack.parse.poly.core

import java.io.File

import com.typesafe.config.{ Config, ConfigFactory }
import org.allenai.common.Config.EnhancedConfig
import org.allenai.nlpstack.core.{ Lemmatized, PostaggedToken, Postagger }
import org.allenai.nlpstack.lemmatize.MorphaStemmer
import org.allenai.nlpstack.parse.poly.ml._
import org.allenai.nlpstack.parse.poly.polytagger.SimplePostaggerInitializer
import org.allenai.nlpstack.postag._
import reming.DefaultJsonProtocol._

import scala.concurrent.{ Await, Future }
import scala.concurrent.duration._
import scala.language.postfixOps

/** Recipe for initializing a sentence tagger.
  *
  * When providing a new implementation of the SentenceTagger interface, please provide an
  * associated SentenceTaggerInitializer here so that it can be used.
  */
trait SentenceTaggerInitializer {
  def initialize(): SentenceTagger
}

/** Initializes a default Factorie part-of-speech tagger.
  *
  * @param useCoarseTags set to true if you want the tagger to output Google coarse POS tags
  */
case class FactoriePostaggerInitializer(useCoarseTags: Boolean) extends SentenceTaggerInitializer {
  override def initialize(): SentenceTagger = {
    NLPStackPostagger(FactoriePostaggerInitializer.factorieTagger, useCoarseTags)
  }
}

object FactoriePostaggerInitializer {
  lazy val factorieTagger = new FactoriePostagger()
}

/** Initializes a default Stanford part-of-speech tagger.
  *
  * @param useCoarseTags set to true if you want the tagger to output Google coarse POS tags
  */
case class StanfordPostaggerInitializer(useCoarseTags: Boolean) extends SentenceTaggerInitializer {
  override def initialize(): SentenceTagger = {
    NLPStackPostagger(StanfordPostaggerInitializer.stanfordTagger, useCoarseTags)
  }
}

object StanfordPostaggerInitializer {
  lazy val stanfordTagger = new StanfordPostagger()
}

case object LexicalPropertiesTaggerInitializer extends SentenceTaggerInitializer {
  override def initialize(): SentenceTagger = {
    IndependentTokenSentenceTagger(LexicalPropertiesTagger)
  }
}

case object TokenPositionTaggerInitializer extends SentenceTaggerInitializer {
  override def initialize(): SentenceTagger = TokenPositionTagger
}

case class BrownClustersTaggerInitializer(clusters: Seq[BrownClusters]) extends SentenceTaggerInitializer {
  override def initialize(): SentenceTagger = {
    IndependentTokenSentenceTagger(BrownClustersTagger(clusters))
  }
}

case class KeywordTaggerInitializer(keywords: Set[String]) extends SentenceTaggerInitializer {
  override def initialize(): SentenceTagger = {
    IndependentTokenSentenceTagger(KeywordTagger(keywords))
  }
}

case class GoogleUnigramTaggerInitializer(tagType: GoogleUnigramTagType) extends SentenceTaggerInitializer {
  override def initialize(): SentenceTagger = {
    IndependentTokenSentenceTagger(GoogleUnigramTagger(
      GoogleUnigramTaggerInitializer.googleNgrams.get, tagType
    ))
  }
}

object GoogleUnigramTaggerInitializer {
  lazy val googleNgrams: Option[DatastoreGoogleNGram] = {
    val taggersConfig =
      ConfigFactory.parseFile(new File(SentenceTagger.taggersConfigFile))
    for {
      googleUnigramConfig <- taggersConfig.get[Config]("googleUnigram")
      groupName <- googleUnigramConfig.get[String]("group")
      artifactName <- googleUnigramConfig.get[String]("name")
      version <- googleUnigramConfig.get[Int]("version")
    } yield {
      new DatastoreGoogleNGram(groupName, artifactName, version, 1000)
    }
  }
}

case object VerbnetTaggerInitializer extends SentenceTaggerInitializer {
  override def initialize(): SentenceTagger = {
    VerbnetTagger(verbnet.get)
  }

  private lazy val verbnet: Option[Verbnet] = {
    val taggersConfig =
      ConfigFactory.parseFile(new File(SentenceTagger.taggersConfigFile))
    for {
      verbnetConfig <- taggersConfig.get[Config]("verbnet")
      groupName <- verbnetConfig.get[String]("group")
      artifactName <- verbnetConfig.get[String]("name")
      version <- verbnetConfig.get[Int]("version")
    } yield {
      new Verbnet(groupName, artifactName, version)
    }
  }
}

object SentenceTaggerInitializer {
  private implicit val factorieInitFormat = jsonFormat1(FactoriePostaggerInitializer.apply)
  private implicit val stanfordInitFormat = jsonFormat1(StanfordPostaggerInitializer.apply)
  private implicit val simpleInitFormat = jsonFormat1(SimplePostaggerInitializer.apply)
  private implicit val lexicalPropertiesTaggerFormat = jsonFormat0(() => LexicalPropertiesTaggerInitializer)
  private implicit val tokenPositionTaggerFormat = jsonFormat0(() => TokenPositionTaggerInitializer)
  private implicit val brownClustersTaggerFormat = jsonFormat1(BrownClustersTaggerInitializer.apply)
  private implicit val keywordTaggerFormat = jsonFormat1(KeywordTaggerInitializer.apply)
  private implicit val googleUnigramTaggerFormat = jsonFormat1(GoogleUnigramTaggerInitializer.apply)
  private implicit val verbnetTaggerFormat = jsonFormat0(() => VerbnetTaggerInitializer)

  implicit val taggerInitJsonFormat = parentFormat[SentenceTaggerInitializer](
    childFormat[FactoriePostaggerInitializer, SentenceTaggerInitializer],
    childFormat[StanfordPostaggerInitializer, SentenceTaggerInitializer],
    childFormat[SimplePostaggerInitializer, SentenceTaggerInitializer],
    childFormat[LexicalPropertiesTaggerInitializer.type, SentenceTaggerInitializer],
    childFormat[TokenPositionTaggerInitializer.type, SentenceTaggerInitializer],
    childFormat[BrownClustersTaggerInitializer, SentenceTaggerInitializer],
    childFormat[KeywordTaggerInitializer, SentenceTaggerInitializer],
    childFormat[GoogleUnigramTaggerInitializer, SentenceTaggerInitializer],
    childFormat[VerbnetTaggerInitializer.type, SentenceTaggerInitializer]
  )
}

/** An interface for any module that takes a Sentence object as input
  * and outputs a TaggedSentence object.
  */
trait SentenceTagger {
  // TODO: extend to handle constraints on tagging
  def tag(sentence: Sentence): TaggedSentence
}

object SentenceTagger {

  val taggersConfigFile = "src/main/resources/featuretaggers.config"

  /** Applies a sequence of sentence taggers to a sentence, tagging each token with the union
    * of their tags.
    *
    * @param sentence the sentence we want to tag
    * @param taggers the taggers we want to apply
    * @return a tagged sentence
    */
  def tagWithMultipleTaggers(sentence: Sentence, taggers: Seq[SentenceTagger]): TaggedSentence = {
    val taggings = taggers map { tagger =>
      tagger.tag(sentence)
    }
    TaggedSentence(
      sentence,
      (Range(0, sentence.tokens.size) map { tokIndex =>
      (tokIndex, taggings.toSet flatMap { tagging: TaggedSentence =>
        tagging.tags.getOrElse(tokIndex, Set())
      })
    }).toMap
    )
  }

  /** Tags all sentences from a given sentence source.
    *
    * Note that this function is multithreaded.
    *
    * @param tagger the tagger to apply
    * @param sentenceSource the sentence source
    * @return an iterator over the resulting tagged sentences
    */
  def tagSentenceSource(
    tagger: SentenceTagger,
    sentenceSource: SentenceSource
  ): Iterator[TaggedSentence] = {

    import scala.concurrent.ExecutionContext.Implicits.global
    val taggingTasks: Iterator[Future[TaggedSentence]] =
      for {
        sentence <- sentenceSource.sentenceIterator
      } yield Future {
        tagger.tag(sentence)
      }
    val futureTagged: Future[Iterator[TaggedSentence]] = Future.sequence(taggingTasks)
    Await.result(futureTagged, 2 days)
  }
}

/** A SentenceTagger that assumes tokens can be independently tagged.
  *
  * @param tokenTagger the token tagger to apply to each token of the sentence
  */
case class IndependentTokenSentenceTagger(tokenTagger: TokenTagger) extends SentenceTagger {

  override def tag(sentence: Sentence): TaggedSentence = {
    TaggedSentence(
      sentence,
      (sentence.tokens.zipWithIndex map { _.swap }).toMap mapValues { tok =>
        tokenTagger.tag(tok)
      }
    )
  }
}

/** A SentenceTagger that tags tokens at key positions in the sentence (specifically the
  * first, second, second-last, last, and nexus tokens).
  */
case object TokenPositionTagger extends SentenceTagger {

  private val featureName = 'place
  private val hasNexusSymbol = 'nexus
  private val hasFirstSymbol = 'first
  private val hasSecondSymbol = 'second
  private val hasSecondLastSymbol = 'secondLast
  private val hasLastSymbol = 'last

  override def tag(sentence: Sentence): TaggedSentence = {
    var tags = Map[Int, Set[TokenTag]]()
    tags = tags.updated(0, Set(TokenTag(featureName, hasNexusSymbol)))
    tags = tags.updated(1, Set(TokenTag(featureName, hasFirstSymbol)))
    tags = tags.updated(2, Set(TokenTag(featureName, hasSecondSymbol)))
    tags = tags.updated(
      sentence.size - 1,
      tags.getOrElse(sentence.size - 1, Set[TokenTag]()) + TokenTag(featureName, hasLastSymbol)
    )
    tags = tags.updated(
      sentence.size - 2,
      tags.getOrElse(sentence.size - 2, Set[TokenTag]()) + TokenTag(featureName, hasSecondLastSymbol)
    )
    val finalTags = tags filterKeys { tokIndex => tokIndex >= 0 && tokIndex < sentence.size }
    TaggedSentence(sentence, finalTags)
  }
}

/** Wrapper for a postagger from NLPStack.
  *
  * @param baseTagger the underlying NLPStack postagger
  * @param useCoarseTags set to true if you want the tagger to produce Google coarse POS tags
  */
case class NLPStackPostagger(baseTagger: Postagger, useCoarseTags: Boolean) extends SentenceTagger {

  override def tag(
    sentence: Sentence
  ): TaggedSentence = {

    val taggedTokens: Map[Int, PostaggedToken] =
      Util.getPostaggedTokens(sentence, baseTagger)
    val tagMap: Map[Int, Set[TokenTag]] = taggedTokens mapValues { tagged =>
      Set(
        if (useCoarseTags) {
          TokenTag(
            'autoCpos,
            Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(tagged.postag, "X"))
          )
        } else {
          TokenTag('autoPos, Symbol(tagged.postag))
        }
      )
    }
    TaggedSentence(sentence, tagMap)
  }
}

/** The FactorieLemmatizer tags the tokens of an input sentence with their lemmas, according
  * to the Factorie lemmatizer.
  */
case object FactorieLemmatizer extends SentenceTagger {

  override def tag(sentence: Sentence): TaggedSentence = {
    val taggedTokens = Util.getPostaggedTokens(sentence, defaultPostagger)
    val lemmaMap: Map[Int, Set[TokenTag]] =
      taggedTokens mapValues { tagged =>
        val lemmatized = Lemmatized[PostaggedToken](tagged, MorphaStemmer.lemmatize(tagged.string, tagged.postag))
        Set(TokenTag('autoLemma, Symbol(lemmatized.lemma)))
      }
    TaggedSentence(sentence, lemmaMap)
  }
}

