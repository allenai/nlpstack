package org.allenai.nlpstack.parse.poly.postagging

import java.io.File
import org.allenai.common.Config.EnhancedConfig
import com.typesafe.config.{ Config, ConfigFactory }
import org.allenai.nlpstack.parse.poly.core.Sentence
import org.allenai.nlpstack.parse.poly.ml._
import org.allenai.nlpstack.postag.{ FactoriePostagger, StanfordPostagger }
import reming.DefaultJsonProtocol._

trait SentenceTagger {
  // TODO: currently no constraints are considered
  def tag(sentence: Sentence): TaggedSentence
}

object SentenceTagger {

  val taggersConfigFile = "src/main/resources/featuretaggers.config"

  private lazy val stanfordTagger = new StanfordPostagger()
  private lazy val factorieTagger = new FactoriePostagger()
  private lazy val googleNgrams: Option[DatastoreGoogleNGram] = {
    val taggersConfig =
      ConfigFactory.parseFile(new File(taggersConfigFile))
    for {
      googleUnigramConfig <- taggersConfig.get[Config]("googleUnigram")
      groupName <- googleUnigramConfig.get[String]("group")
      artifactName <- googleUnigramConfig.get[String]("name")
      version <- googleUnigramConfig.get[Int]("version")
    } yield {
      new DatastoreGoogleNGram(groupName, artifactName, version, 1000)
    }
  }
  private lazy val verbnet: Option[Verbnet] = {
    val taggersConfig =
      ConfigFactory.parseFile(new File(taggersConfigFile))
    for {
      verbnetConfig <- taggersConfig.get[Config]("verbnet")
      groupName <- verbnetConfig.get[String]("group")
      artifactName <- verbnetConfig.get[String]("name")
      version <- verbnetConfig.get[Int]("version")
    } yield {
      new Verbnet(groupName, artifactName, version)
    }
  }

  private lazy val wikiSet = new WikiSet("/Users/markhopkins/Projects/data/monolingual/enwiki-latest-all-titles-in-ns0")

  /** Initializes a part-of-speech tagger.
    *
    * @param initializer the initialization recipe
    * @return the initialized tagger
    */
  def initialize(initializer: SentenceTaggerInitializer): SentenceTagger = {
    initializer match {
      case FactoriePostaggerInitializer(useCoarseTags) =>
        NLPStackPostagger(factorieTagger, useCoarseTags)
      case StanfordPostaggerInitializer(useCoarseTags) =>
        NLPStackPostagger(stanfordTagger, useCoarseTags)
      case SimplePostaggerInitializer(configFile) =>
        SimplePostagger.load(configFile)
      case LexicalPropertiesTaggerInitializer =>
        IndependentTokenSentenceTagger(LexicalPropertiesTagger)
      case TokenPositionTaggerInitializer =>
        TokenPositionTagger
      case BrownClustersTaggerInitializer(clusters) =>
        IndependentTokenSentenceTagger(BrownClustersTagger(clusters))
      case KeywordTaggerInitializer(keywords) =>
        IndependentTokenSentenceTagger(KeywordTagger(keywords))
      case GoogleUnigramTaggerInitializer(tagType) =>
        IndependentTokenSentenceTagger(GoogleUnigramTagger(googleNgrams.get, tagType))
      case WikiSetTaggerInitializer =>
        WikiSetTagger(wikiSet)
      case VerbnetTaggerInitializer =>
        VerbnetTagger(verbnet.get)
    }
  }

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
}

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

case object TokenPositionTagger extends SentenceTagger {

  private val featureName = 'place
  private val hasNexusSymbol = 'nexus
  private val hasFirstSymbol = 'first
  private val hasSecondSymbol = 'second
  private val hasSecondLastSymbol = 'secondLast
  private val hasLastSymbol = 'last

  override def tag(sentence: Sentence): TaggedSentence = {
    TaggedSentence(
      sentence,
      (Range(0, sentence.tokens.size) map {
      case tokenIndex =>
        (
          tokenIndex,
          Set(
            if (tokenIndex == 0) Some(TokenTag(featureName, hasNexusSymbol)) else None,
            if (tokenIndex == 1) Some(TokenTag(featureName, hasFirstSymbol)) else None,
            if (tokenIndex == 2) Some(TokenTag(featureName, hasSecondSymbol)) else None,
            if (tokenIndex == sentence.size - 2) Some(TokenTag(featureName, hasSecondLastSymbol)) else None,
            if (tokenIndex == sentence.size - 1) Some(TokenTag(featureName, hasLastSymbol)) else None
          ).flatten
        )
    }).toMap
    )
  }
}

/** Recipe for initializing a part-of-speech tagger. */
sealed trait SentenceTaggerInitializer

/** Initializes a default Factorie part-of-speech tagger.
  *
  * @param useCoarseTags set to true if you want the tagger to output Google coarse POS tags
  */
case class FactoriePostaggerInitializer(useCoarseTags: Boolean) extends SentenceTaggerInitializer

/** Initializes a default Stanford part-of-speech tagger.
  *
  * @param useCoarseTags set to true if you want the tagger to output Google coarse POS tags
  */
case class StanfordPostaggerInitializer(useCoarseTags: Boolean) extends SentenceTaggerInitializer

/** Initializes a SimplePostagger.
  *
  * @param configFile filename containing the JSON configuration
  */
case class SimplePostaggerInitializer(configFile: String) extends SentenceTaggerInitializer

case object LexicalPropertiesTaggerInitializer extends SentenceTaggerInitializer
case object TokenPositionTaggerInitializer extends SentenceTaggerInitializer
case class BrownClustersTaggerInitializer(clusters: Seq[BrownClusters]) extends SentenceTaggerInitializer
case class KeywordTaggerInitializer(keywords: Set[String]) extends SentenceTaggerInitializer
case class GoogleUnigramTaggerInitializer(tagType: GoogleUnigramTagType) extends SentenceTaggerInitializer
case object WikiSetTaggerInitializer extends SentenceTaggerInitializer
case object VerbnetTaggerInitializer extends SentenceTaggerInitializer

object SentenceTaggerInitializer {
  private implicit val factorieInitFormat = jsonFormat1(FactoriePostaggerInitializer.apply)
  private implicit val stanfordInitFormat = jsonFormat1(StanfordPostaggerInitializer.apply)
  private implicit val simpleInitFormat = jsonFormat1(SimplePostaggerInitializer.apply)
  private implicit val lexicalPropertiesTaggerFormat = jsonFormat0(() => LexicalPropertiesTaggerInitializer)
  private implicit val tokenPositionTaggerFormat = jsonFormat0(() => TokenPositionTaggerInitializer)
  private implicit val brownClustersTaggerFormat = jsonFormat1(BrownClustersTaggerInitializer.apply)
  private implicit val keywordTaggerFormat = jsonFormat1(KeywordTaggerInitializer.apply)
  private implicit val googleUnigramTaggerFormat = jsonFormat1(GoogleUnigramTaggerInitializer.apply)
  private implicit val wikiSetTaggerFormat = jsonFormat0(() => WikiSetTaggerInitializer)
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
    childFormat[WikiSetTaggerInitializer.type, SentenceTaggerInitializer],
    childFormat[VerbnetTaggerInitializer.type, SentenceTaggerInitializer]
  )
}

