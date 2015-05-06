package org.allenai.nlpstack.parse.poly.core

import org.allenai.nlpstack.parse.poly.postagging.{ PolyPostaggerInitializer, PolyPostagger }
import org.allenai.nlpstack.core.{
  Token => NLPStackToken,
  Lemmatized,
  PostaggedToken,
  Postagger,
  Tokenizer
}
import org.allenai.nlpstack.parse.poly.ml.{
  BrownClusters,
  DatastoreGoogleNGram,
  GoogleUnigram,
  Verbnet
}
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.lemmatize._

import reming.DefaultJsonProtocol._

sealed trait GoogleUnigramTagType {
  val name: String
}
case object GoogleUnigramPos extends GoogleUnigramTagType {
  val name: String = "posTag"
}
case object GoogleUnigramCpos extends GoogleUnigramTagType {
  val name: String = "cposTag"
}

object GoogleUnigramTagType {
  private implicit val googleUnigramPosFormat = jsonFormat0(() => GoogleUnigramPos)
  private implicit val googleUnigramCposFormat = jsonFormat0(() => GoogleUnigramCpos)

  implicit val unigramTagTypeJsonFormat = parentFormat[GoogleUnigramTagType](
    childFormat[GoogleUnigramPos.type, GoogleUnigramTagType],
    childFormat[GoogleUnigramCpos.type, GoogleUnigramTagType]
  )
}

trait SentenceTransform {
  def transform(sentence: Sentence): Sentence
}

object SentenceTransform {
  private implicit val lexicalPropertiesTaggerFormat = jsonFormat0(() => LexicalPropertiesTagger)
  private implicit val polyPostaggerFormat = jsonFormat1(PolyPostaggerSentenceTransform.apply)
  private implicit val brownClustersTaggerFormat = jsonFormat1(BrownClustersTagger.apply)
  private implicit val verbnetTaggerFormat = jsonFormat1(VerbnetTagger.apply)
  private implicit val googleUnigramTaggerFormat =
    jsonFormat2(GoogleUnigramTagger.apply)

  implicit val sentenceTransformJsonFormat = parentFormat[SentenceTransform](
    childFormat[LexicalPropertiesTagger.type, SentenceTransform],
    childFormat[PolyPostaggerSentenceTransform, SentenceTransform],
    childFormat[BrownClustersTagger, SentenceTransform],
    childFormat[VerbnetTagger, SentenceTransform],
    childFormat[GoogleUnigramTagger, SentenceTransform]
  )

  /** Given a Sentences, produce a seq of PostaggedTokens.
    */
  def getPostaggedTokens(sentence: Sentence, posTagger: Postagger): IndexedSeq[PostaggedToken] = {
    val words: IndexedSeq[String] = sentence.tokens.tail map { tok => tok.word.name }
    val nlpStackTokens: IndexedSeq[NLPStackToken] =
      Tokenizer.computeOffsets(words, words.mkString).toIndexedSeq
    posTagger.postagTokenized(nlpStackTokens).toIndexedSeq
  }
}

/** The PolyPostaggerSentenceTransform tags an input sentence with automatic part-of-speech tags
  * from a tagger implementing the PolyPostagger interface.
  *
  * TODO: change this to allow key name to be specified
  */
case class PolyPostaggerSentenceTransform(
    taggerInit: PolyPostaggerInitializer
) extends SentenceTransform {

  @transient private val baseTagger = PolyPostagger.initializePostagger(taggerInit)

  def transform(sentence: Sentence): Sentence = {
    val tagged: Option[TaggedSentence] = baseTagger.tag(sentence)
    require(tagged != None)
    val taggedSent: Sentence = tagged.get.addTagsToSentenceProperties('autoPos)
    Sentence(NexusToken +: taggedSent.tokens.tail map {
      case posTagged =>
        val autoCpos = posTagged.getProperty('autoPos) map { posTag =>
          Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(
            posTag.name, "X"
          ))
        }
        posTagged.updateProperties(Map(
          'autoCpos -> autoCpos
        ))
    })
  }
}

/** The LexicalPropertiesTagger tags the tokens of an input sentence with lexical
  * properties like whether the first letter is capitalized, or whether it contains numeric
  * digits.
  */
case object LexicalPropertiesTagger extends SentenceTransform {

  def transform(sentence: Sentence): Sentence = {
    Sentence(sentence.tokens map { tok =>
      val tokStr = tok.word.name
      val firstLetterCapital = tokStr.headOption match {
        case Some(x) if Character.isUpperCase(x) => Set('firstCap)
        case _ => Set[Symbol]()
      }
      val existsCapital = tokStr match {
        case tokStr: String if tokStr exists { Character.isUpperCase(_) } => Set('existsCap)
        case _ => Set[Symbol]()
      }
      val allCaps = tokStr match {
        case tokStr: String if tokStr forall { Character.isUpperCase(_) } => Set('allCaps)
        case _ => Set[Symbol]()
      }
      val existsNumber = tokStr match {
        case tokStr: String if tokStr exists { Character.isDigit(_) } => Set('existsNum)
        case _ => Set[Symbol]()
      }
      tok.updateProperties(Map(
        'lcase -> Set(Symbol(tokStr.toLowerCase)),
        'lexical -> (firstLetterCapital ++ existsCapital ++ allCaps ++ existsNumber)
      ))
    })
  }
}

/** The BrownClustersTagger tags the tokens of a sentence with their Brown clusters. */
case class BrownClustersTagger(clusters: Seq[BrownClusters]) extends SentenceTransform {

  def transform(sentence: Sentence): Sentence = {
    Sentence(for {
      tok <- sentence.tokens
    } yield tok.updateProperties((for {
      (cluster, clusterId) <- clusters.zipWithIndex
    } yield {
      Symbol(s"brown${clusterId}") ->
        Set(cluster.getMostSpecificCluster(Symbol(tok.word.name.toLowerCase)))
    }).toMap))
  }
}

/** The FactorieLemmatizer tags the tokens of an input sentence with their lemmas, according
  * to the Factorie lemmatizer.
  */
case object FactorieLemmatizer extends SentenceTransform {

  override def transform(sentence: Sentence): Sentence = {
    val taggedTokens = SentenceTransform.getPostaggedTokens(sentence, defaultPostagger)
    val lemmatizedTaggedTokens: IndexedSeq[Lemmatized[PostaggedToken]] =
      taggedTokens map {
        x => Lemmatized[PostaggedToken](x, MorphaStemmer.lemmatize(x.string, x.postag))
      }
    Sentence(NexusToken +: (lemmatizedTaggedTokens.zip(sentence.tokens.tail) map {
      case (tagged, untagged) =>
        untagged.updateProperties(Map(
          'factorieLemma -> Set(Symbol(tagged.lemma))
        ))
    }))
  }
}

/** The VerbnetTagger tags the tokens of an input sentence with their Verbnet classes.
  */
case class VerbnetTagger(verbnet: Verbnet) extends SentenceTransform {
  override def transform(sentence: Sentence): Sentence = {
    val lemmatized = FactorieLemmatizer.transform(sentence)
    Sentence(for {
      tok <- lemmatized.tokens
    } yield {
      val tokLemmaLC = tok.getDeterministicProperty('factorieLemma).name.toLowerCase
      val tokVerbnetPrimaryFrames = verbnet.getVerbnetFramePrimaryNames(tokLemmaLC)
      tok.updateProperties(Map('verbnetPrimaryFrames -> tokVerbnetPrimaryFrames))
    })
  }
}

/** Frequency Distribution of POS tags for words based on Google Ngram's Nodes (unigrams).
  */
case class GoogleUnigramTagger(
    googleNgram: DatastoreGoogleNGram,
    tagType: GoogleUnigramTagType
) extends SentenceTransform {

  override def transform(sentence: Sentence): Sentence = {
    Sentence(NexusToken +: (sentence.tokens.tail map {
      tok =>
        val tagFreqMap: Map[String, Double] = tagType match {
          case GoogleUnigramPos =>
            GoogleUnigram.getNormalizedPostagDistribution(
              tok.word.name, googleNgram.ngramMap, googleNgram.frequencyCutoff
            )
          case GoogleUnigramCpos =>
            GoogleUnigram.getNormalizedCpostagDistribution(
              tok.word.name, googleNgram.ngramMap, googleNgram.frequencyCutoff
            )
        }
        // Create feature for each dependency label based on the normalized frequency
        // bucket it lies in.
        val frequencyFeatureMap: Map[Symbol, Set[Symbol]] = (for {
          tag <- tagFreqMap.keySet
        } yield {
          val normalizedFrequency = tagFreqMap(tag)
          val symbolSetWithCurrentPostag = Set(Symbol(tag))
          val freqBucket =
            tagType.name + GoogleUnigram.getFrequencyBucketForFeature(normalizedFrequency)
          (Symbol(freqBucket), symbolSetWithCurrentPostag)
        }).groupBy(_._1).mapValues(_.flatMap(v => v._2))
        val bestTagMapping: Map[Symbol, Set[Symbol]] =
          if (tagFreqMap.nonEmpty) {
            val mostLikelyTag = (tagFreqMap maxBy {
              _._2
            })._1
            Map(Symbol(s"${tagType.name}MostLikely") -> Set(Symbol(mostLikelyTag)))
          } else {
            Map()
          }
        tok.updateProperties(frequencyFeatureMap ++ bestTagMapping)
    }))
  }
}
