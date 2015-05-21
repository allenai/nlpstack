package org.allenai.nlpstack.parse.poly.postagging

import org.allenai.common.Config.EnhancedConfig
import com.typesafe.config.{ Config, ConfigFactory }

import org.allenai.nlpstack.parse.poly.core._
import org.allenai.nlpstack.parse.poly.fsm.{ SculptureSource, Sculpture }
import org.allenai.nlpstack.parse.poly.ml._
import org.allenai.nlpstack.parse.poly.polyparser.PolytreeParseSource
import org.allenai.nlpstack.postag._

import java.io.File

import reming.DefaultJsonProtocol._

case class SentenceTagging(sentence: Sentence, tags: Map[Int, Set[TokenTag]]) extends Sculpture {
  override val marbleBlock = sentence
}

/** A data source for TaggedSentence objects. */
trait TaggedSentenceSource extends SculptureSource with SentenceSource {
  def taggedSentenceIterator: Iterator[SentenceTagging]

  override def sculptureIterator: Iterator[Sculpture] = taggedSentenceIterator

  override def sentenceIterator: Iterator[Sentence] = taggedSentenceIterator map { taggedSentence =>
    taggedSentence.sentence
  }
}

/** A TaggedSentenceSource derived from a PolytreeParseSource.
  *
  * Tokens are tagged with a specified property from their `properties` field.
  *
  * @param parseSource the parse source to derive the tagged sentences from
  * @param propertyName the token property to use as the "tag"
  */
case class ParseDerivedTaggedSentenceSource(
    parseSource: PolytreeParseSource,
    propertyName: Symbol
) extends TaggedSentenceSource {

  override def taggedSentenceIterator: Iterator[SentenceTagging] = {
    for {
      sentence <- parseSource.sentenceIterator
    } yield {
      SentenceTagging(
        sentence,
        (sentence.tokens.zipWithIndex map {
        case (tok, index) =>
          (index, tok.getProperty(propertyName) map { prop => TokenTag(propertyName, prop) })
      }).toMap
      )
    }
  }
}

trait SentenceTagger {
  // TODO: currently no constraints are considered
  def tag(sentence: Sentence): SentenceTagging
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

  def tagWithMultipleTaggers(sentence: Sentence, taggers: Seq[SentenceTagger]): SentenceTagging = {
    val taggings = taggers map { tagger =>
      tagger.tag(sentence)
    }
    SentenceTagging(
      sentence,
      (Range(0, sentence.tokens.size) map { tokIndex =>
      (tokIndex, taggings.toSet flatMap { tagging: SentenceTagging =>
        tagging.tags.getOrElse(tokIndex, Set())
      })
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
    childFormat[BrownClustersTaggerInitializer, SentenceTaggerInitializer],
    childFormat[KeywordTaggerInitializer, SentenceTaggerInitializer],
    childFormat[GoogleUnigramTaggerInitializer, SentenceTaggerInitializer],
    childFormat[WikiSetTaggerInitializer.type, SentenceTaggerInitializer],
    childFormat[VerbnetTaggerInitializer.type, SentenceTaggerInitializer]
  )
}

case class IndependentTokenSentenceTagger(tokenTagger: TokenTagger) extends SentenceTagger {

  override def tag(sentence: Sentence): SentenceTagging = {
    SentenceTagging(
      sentence,
      (sentence.tokens.zipWithIndex map { _.swap }).toMap mapValues { tok =>
        tokenTagger.tag(tok)
      }
    )
  }
}

case class WikiSetTagger(wikiset: WikiSet) extends SentenceTagger {

  override def tag(sentence: Sentence): SentenceTagging = {
    val ngrams: Set[(Int, Int)] = wikiset.identifyNgrams(sentence, 2)
    val ngramBeginnings = ngrams map { _._1 }
    val ngramEndings = ngrams map { _._2 }
    val ngramInternals: Set[Int] = ngrams flatMap { case (start, finish) => Range(start + 1, finish) }

    SentenceTagging(
      sentence,
      (Range(1, sentence.tokens.size) map { tokIndex =>
      (
        tokIndex,
        Set(
          if (ngramBeginnings.contains(tokIndex)) { Some(TokenTag('wiki, 'B)) } else { None },
          if (ngramEndings.contains(tokIndex)) { Some(TokenTag('wiki, 'E)) } else { None },
          if (ngramInternals.contains(tokIndex)) { Some(TokenTag('wiki, 'I)) } else { None }
        ).flatten
      )
    }).toMap
    )
  }
}

case class VerbnetTagger(verbnet: Verbnet) extends SentenceTagger {
  override def tag(sentence: Sentence): SentenceTagging = {
    val lemmatized = NLPStackLemmatizer.tag(sentence)
    SentenceTagging(
      sentence,
      lemmatized.tags mapValues {
        case tags =>
          val tokLemmaLC = tags.head.value.name
          val tokVerbnetFrames: Set[Symbol] = verbnet.getVerbnetFramePrimaryNames(tokLemmaLC)
          tokVerbnetFrames map { frame =>
            TokenTag('verbnetFrame, frame)
          }
      }
    )
  }
}
