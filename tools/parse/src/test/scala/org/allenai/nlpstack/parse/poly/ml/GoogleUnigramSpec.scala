package org.allenai.nlpstack.parse.poly.ml

import org.allenai.common.Config.EnhancedConfig
import org.allenai.common.Logging
import org.allenai.common.testkit.UnitSpec
import org.allenai.datastore._
import org.allenai.nlpstack.core.PostaggedToken
import org.allenai.nlpstack.postag._
import org.allenai.nlpstack.parse.poly.core.{
  FactorieSentenceTagger,
  GoogleUnigramTagger,
  Sentence,
  SentenceTransform,
  Token
}

import com.typesafe.config.{ Config, ConfigFactory }
import java.io.{ File, FileWriter, Writer }

class GoogleNGramSpec extends UnitSpec with Logging {

  val taggersConfigPath = "src/main/resources/featuretaggers.config"

  val taggersConfig = ConfigFactory.parseFile(new File(taggersConfigPath))

  val googleUnigramConfig = taggersConfig.getConfig("googleUnigram")
  val groupName = googleUnigramConfig.getString("group")
  val artifactName = googleUnigramConfig.getString("name")
  val version = googleUnigramConfig.getInt("version")

  val frequencyCutoff = 1000
  val googleUnigram = new GoogleNGram(groupName, artifactName, version, frequencyCutoff)

  val sentence1 = Sentence(
    IndexedSeq(Token('The), Token('tiger), Token('is), Token('roaring))
  )

  val taggedTokens = SentenceTransform.getPostaggedTokens(sentence1, defaultPostagger)

  "GoogleNgram.ngramMap" should
    "return the correct syntactic ngrams and associated frequencies for a given word" in {
      googleUnigram.ngramMap(taggedTokens(2).string) shouldBe Seq(
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "amod", 0)), 67769),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "nn", 0)), 33116),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "xcomp", 0)), 30578),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "conj", 0)), 22531),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "pobj", 0)), 22023),
        NgramInfo(Seq(SyntacticInfo("roaring", "NNP", "nn", 0)), 21594),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "ROOT", 0)), 17249),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "dep", 0)), 15944),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "partmod", 0)), 15624),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "nsubj", 0)), 13926),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "conj", 0)), 11338),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "dobj", 0)), 11249),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "pcomp", 0)), 10641),
        NgramInfo(Seq(SyntacticInfo("roaring", "JJ", "amod", 0)), 8485),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "ccomp", 0)), 5162),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "pobj", 0)), 2826),
        NgramInfo(Seq(SyntacticInfo("roaring", "JJ", "dep", 0)), 2768),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "nsubjpass", 0)), 2471),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "nn", 0)), 2356),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "advcl", 0)), 2342),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "dep", 0)), 2286),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "attr", 0)), 1944),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "rcmod", 0)), 1878),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "amod", 0)), 1817),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "appos", 0)), 1712),
        NgramInfo(Seq(SyntacticInfo("roaring", "NN", "ROOT", 0)), 1413),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "dobj", 0)), 1056),
        NgramInfo(Seq(SyntacticInfo("roaring", "VBG", "nsubj", 0)), 1047)
      )
    }

  "GoogleUnigramTagger.transform" should
    "return the correct feature value set for a given token" in {
      val googleUnigramTagger = GoogleUnigramTagger(googleUnigram)
      googleUnigramTagger.transform(FactorieSentenceTagger.transform(sentence1)) shouldBe
        Sentence(IndexedSeq(
          Token('nexus, Map(
            'lcase -> Set('nexus),
            'cpos -> Set('nexus)
          )),
          Token('tiger, Map(
            'autoPos -> Set('NN),
            'autoCpos -> Set('NOUN)
          )),
          Token('is, Map(
            'depLabelFreq11to20 -> Set('ccomp),
            'depLabelFreq1to10 -> Set('advcl, 'rcmod, 'conj, 'dep),
            'depLabelFreq61to70 -> Set('ROOT),
            'autoCpos -> Set('VERB),
            'autoPos -> Set('VBZ),
            'depLabelFreqBelow1 -> Set('pcomp, 'pred, 'nsubj, 'parataxis),
            'depLabelFreqSmall -> Set('infmod, 'iobj, 'dobj, 'advmod, 'prep, 'nsubjpass, 'csubj,
              'purpcl, 'amod, 'tmod, 'xcomp, 'attr, 'acomp, 'partmod, 'nn, 'cc, 'csubjpass,
              'appos, 'pobj)
          )),
          Token('roaring, Map(
            'depLabelFreq11to20 -> Set('xcomp, 'conj),
            'depLabelFreq1to10 -> Set('pcomp, 'dep, 'ccomp, 'partmod, 'ROOT),
            'autoCpos -> Set('VERB),
            'autoPos -> Set('VBG),
            'depLabelFreqBelow1 -> Set('rcmod, 'advcl, 'pobj, 'nn),
            'depLabelFreqSmall -> Set('nsubj, 'dobj),
            'depLabelFreq31to40 -> Set('amod)
          ))
        ))
    }

}