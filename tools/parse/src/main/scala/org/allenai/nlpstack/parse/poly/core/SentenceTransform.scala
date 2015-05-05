package org.allenai.nlpstack.parse.poly.core

import org.allenai.nlpstack.parse.poly.postagging.SimplePostagger
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
  GoogleNGram,
  GoogleUnigram,
  NgramInfo,
  Verbnet
}
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.lemmatize._

import reming.DefaultJsonProtocol._

trait SentenceTransform {
  def transform(sentence: Sentence): Sentence
}

object SentenceTransform {
  private implicit val factorieSentenceTaggerFormat = jsonFormat0(() => FactorieSentenceTagger)
  private implicit val stanfordSentenceTaggerFormat = jsonFormat0(() => StanfordSentenceTagger)
  private implicit val lexicalPropertiesTaggerFormat = jsonFormat0(() => LexicalPropertiesTagger)
  private implicit val brownClustersTaggerFormat = jsonFormat1(BrownClustersTagger.apply)
  private implicit val verbnetTaggerFormat = jsonFormat1(VerbnetTagger.apply)
  private implicit val googleUnigramDepLabelTaggerFormat =
    jsonFormat1(GoogleUnigramDepLabelTagger.apply)
  private implicit val googleUnigramPostagTaggerFormat =
    jsonFormat1(GoogleUnigramPostagTagger.apply)

  implicit val sentenceTransformJsonFormat = parentFormat[SentenceTransform](
    childFormat[FactorieSentenceTagger.type, SentenceTransform],
    childFormat[StanfordSentenceTagger.type, SentenceTransform],
    childFormat[LexicalPropertiesTagger.type, SentenceTransform],
    childFormat[BrownClustersTagger, SentenceTransform],
    childFormat[VerbnetTagger, SentenceTransform],
    childFormat[GoogleUnigramDepLabelTagger, SentenceTransform],
    childFormat[GoogleUnigramPostagTagger, SentenceTransform]
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

/** The FactorieSentenceTagger tags an input sentence with automatic part-of-speech tags
  * from the Factorie tagger.
  */
case object FactorieSentenceTagger extends SentenceTransform {

  val baseTagger = SimplePostagger.load("/Users/markhopkins/Projects/experiments/parsing/temp/qbank.tagger.json")
  //val baseTagger = NLPStackPostagger(defaultPostagger)

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

    //val taggedTokens = SentenceTransform.getPostaggedTokens(sentence, defaultPostagger)
    /*
    Sentence(NexusToken +: (taggedTokens.zip(sentence.tokens.tail) map {
      case (tagged, untagged) =>
        val autoPos = Symbol(tagged.postag)
        val autoCpos = Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(
          autoPos.name, "X"
        ))
        untagged.updateProperties(Map(
          'autoPos -> Set(autoPos),
          'autoCpos -> Set(autoCpos)
        ))
    }))
    */
  }
}

/** The StanfordSentenceTagger tags an input sentence with automatic part-of-speech tags
  * from the Stanford tagger.
  */
case object StanfordSentenceTagger extends SentenceTransform {

  @transient private val stanfordTagger = new StanfordPostagger()

  def transform(sentence: Sentence): Sentence = {
    val taggedTokens = SentenceTransform.getPostaggedTokens(sentence, stanfordTagger)
    Sentence(NexusToken +: (taggedTokens.zip(sentence.tokens.tail) map {
      case (tagged, untagged) =>
        val autoPos = Symbol(tagged.postag)
        val autoCpos = Symbol(WordClusters.ptbToUniversalPosTag.getOrElse(
          autoPos.name, "X"
        ))
        untagged.updateProperties(Map(
          'autoPos -> Set(autoPos),
          'autoCpos -> Set(autoCpos)
        ))
    }))
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

/** Frequency Distribution of dependency labels for tokens based on Google Ngram's Nodes (unigrams).
  */
/*
case class GoogleUnigramDepLabelTagger(googleNgram: DatastoreGoogleNGram) extends SentenceTransform {

  @transient private val stanfordTagger = new StanfordPostagger()

  override def transform(sentence: Sentence): Sentence = {
    val taggedTokens = SentenceTransform.getPostaggedTokens(sentence, defaultPostagger)
    Sentence(NexusToken +: (taggedTokens.zip(sentence.tokens.tail) map {
      case (tagged, untagged) =>
        val depLabelFreqMap = GoogleUnigram.getDepLabelNormalizedDistribution(
          tagged, googleNgram.ngramMap, googleNgram.frequencyCutoff
        )
        // Create feature for each dependency label based on the normalized frequency
        // bucket it lies in.
        val frequencyFeatureMap: Map[Symbol, Set[Symbol]] = (for {
          depLabel <- depLabelFreqMap.keySet
        } yield {
          val normalizedFrequency = depLabelFreqMap(depLabel)
          val symbolSetWithCurrentDepLabel = Set(Symbol(depLabel))
          val freqBucket =
            "depLabel" + GoogleUnigram.getFrequencyBucketForFeature(normalizedFrequency)
          (Symbol(freqBucket), symbolSetWithCurrentDepLabel)
        }).groupBy(_._1).mapValues(_.flatMap(v => v._2))
        untagged.updateProperties(frequencyFeatureMap)
    }))
  }
}
*/
/** Frequency Distribution of dependency labels for tokens based on Google Ngram's Nodes (unigrams).
  */

case class GoogleUnigramDepLabelTagger(googleNgram: DatastoreGoogleNGram) extends SentenceTransform {

  override def transform(sentence: Sentence): Sentence = {
    val nextSent = transformPos(sentence)
    //val taggedTokens = SentenceTransform.getPostaggedTokens(nextSent, defaultPostagger)
    Sentence(NexusToken +: (nextSent.tokens.zip(nextSent.tokens.tail) map {
      case (tagged, untagged) =>
        val tagFreqMap = GoogleUnigram.getNormalizedCpostagDistribution(
          tagged.word.name, googleNgram.ngramMap, googleNgram.frequencyCutoff
        )
        // Create feature for each dependency label based on the normalized frequency
        // bucket it lies in.
        val frequencyFeatureMap: Map[Symbol, Set[Symbol]] = (for {
          tagLabel <- tagFreqMap.keySet
        } yield {
          val normalizedFrequency = tagFreqMap(tagLabel)
          val symbolSetWithCurrentDepLabel = Set(Symbol(tagLabel))

          if (normalizedFrequency >= 0.95) {
            Some(('tagFreqDominant, symbolSetWithCurrentDepLabel))
          } else if (normalizedFrequency >= 0.5) {
            Some(('tagFreqBelow95, symbolSetWithCurrentDepLabel))
          } else if (normalizedFrequency >= 0.1) {
            Some(('tagFreqBelow50, symbolSetWithCurrentDepLabel))
          } else if (normalizedFrequency >= 0.01) {
            Some(('tagFreqBelow10, symbolSetWithCurrentDepLabel))
          } else {
            None
          }
        }).flatten.groupBy(_._1).mapValues(_.flatMap(v => v._2))

        val bestTagMapping: Map[Symbol, Set[Symbol]] =
          if (tagFreqMap.nonEmpty) {
            val mostLikelyTag = (tagFreqMap maxBy {
              _._2
            })._1
            Map('mostLikelyTag -> Set(Symbol(mostLikelyTag)))
          } else {
            Map()
          }

        untagged.updateProperties(frequencyFeatureMap ++ bestTagMapping)
    }))
  }

  private def transformPos(sentence: Sentence): Sentence = {

    val taggedTokens = SentenceTransform.getPostaggedTokens(sentence, defaultPostagger)
    Sentence(NexusToken +: (taggedTokens.zip(sentence.tokens.tail) map {
      case (tagged, untagged) =>
        val posFreqMap = GoogleUnigram.getNormalizedPostagDistribution(
          tagged.string, googleNgram.ngramMap, googleNgram.frequencyCutoff
        )
        // Create feature for each dependency label based on the normalized frequency
        // bucket it lies in.
        val frequencyFeatureMap: Map[Symbol, Set[Symbol]] = (for {
          cposLabel <- posFreqMap.keySet
        } yield {
          val normalizedFrequency = posFreqMap(cposLabel)
          val symbolSetWithCurrentDepLabel = Set(Symbol(cposLabel))

          if (normalizedFrequency >= 0.8) {
            Some(('posFreqDominant, symbolSetWithCurrentDepLabel))
          } else if (normalizedFrequency >= 0.2) {
            Some(('posFreqBelow80, symbolSetWithCurrentDepLabel))
          } else if (normalizedFrequency >= 0.05) {
            Some(('posFreqBelow20, symbolSetWithCurrentDepLabel))
          } else if (normalizedFrequency >= 0.01) {
            Some(('posFreqBelow5, symbolSetWithCurrentDepLabel))
          } else {
            None
          }
        }).flatten.groupBy(_._1).mapValues(_.flatMap(v => v._2))

        val bestTagMapping: Map[Symbol, Set[Symbol]] =
          if (posFreqMap.nonEmpty) {
            val mostLikelyTag = (posFreqMap maxBy {
              _._2
            })._1
            Map('mostLikelyPos -> Set(Symbol(mostLikelyTag)))
          } else {
            Map()
          }

        untagged.updateProperties(frequencyFeatureMap ++ bestTagMapping)
    }))
  }

}

/** Frequency Distribution of POS tags for words based on Google Ngram's Nodes (unigrams).
  */
case class GoogleUnigramPostagTagger(googleNgram: DatastoreGoogleNGram) extends SentenceTransform {

  override def transform(sentence: Sentence): Sentence = {
    Sentence(NexusToken +: ((sentence.tokens.tail) map {
      tok =>
        val postagFreqMap = GoogleUnigram.getPosTagNormalizedDistribution(
          tok.word.name, googleNgram.ngramMap, googleNgram.frequencyCutoff
        )
        // Create feature for each dependency label based on the normalized frequency
        // bucket it lies in.
        val frequencyFeatureMap: Map[Symbol, Set[Symbol]] = (for {
          postag <- postagFreqMap.keySet
        } yield {
          val normalizedFrequency = postagFreqMap(postag)
          val symbolSetWithCurrentPostag = Set(Symbol(postag))
          val freqBucket =
            "posTag" + GoogleUnigram.getFrequencyBucketForFeature(normalizedFrequency)
          (Symbol(freqBucket), symbolSetWithCurrentPostag)
        }).groupBy(_._1).mapValues(_.flatMap(v => v._2))
        tok.updateProperties(frequencyFeatureMap)
    }))
  }
}
