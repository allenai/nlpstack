package org.allenai.nlpstack.parse.poly.ml

import org.allenai.common.Config.EnhancedConfig
import org.allenai.common.Logging
import org.allenai.common.testkit.UnitSpec
import org.allenai.datastore._
import org.allenai.nlpstack.parse.poly.core.{ PolyPostaggerSentenceTransform, VerbnetTagger, Token, Sentence }

import com.typesafe.config.{ Config, ConfigFactory }
import java.io.{ File, FileWriter, Writer }

import org.allenai.nlpstack.parse.poly.postagging.FactoriePostaggerInitializer

class VerbnetUtilSpec extends UnitSpec with Logging {

  val taggersConfigPath = "src/main/resources/featuretaggers.config"

  val taggersConfig = ConfigFactory.parseFile(new File(taggersConfigPath))

  val verbnetConfig = taggersConfig.getConfig("verbnet")
  val groupName = verbnetConfig.getString("group")
  val artifactName = verbnetConfig.getString("name")
  val version = verbnetConfig.getInt("version")

  val verbnet = new Verbnet(groupName, artifactName, version)

  "VerbnetUtil.getVerbnetClasses" should
    "return the correct answer for verbs present in VerbNet" in {
      verbnet.getVerbnetClassNames("roar") shouldBe Set(
        Symbol("run-51.3.2"),
        Symbol("weather-57"),
        Symbol("animal_sounds-38"),
        Symbol("manner_speaking-37.3"),
        Symbol("sound_emission-43.2")
      )
      verbnet.getVerbnetClassNames("boast") shouldBe Set(Symbol("complain-37.8"))
      verbnet.getVerbnetClassNames("synthesize") shouldBe Set(Symbol("create-26.4"))
      verbnet.getVerbnetClassNames("run") shouldBe Set(
        Symbol("swarm-47.5.1-1"),
        Symbol("meander-47.7"),
        Symbol("carry-11.4"),
        Symbol("preparing-26.3-1"),
        Symbol("run-51.3.2-2-1"),
        Symbol("bump-18.4")
      )
    }

  "VerbnetUtil.getVerbnetClasses" should
    "return the correct answer for words NOT present in VerbNet" in {
      verbnet.getVerbnetClassNames("synthesis") shouldBe Set()
      verbnet.getVerbnetClassNames("apple") shouldBe Set()
    }

  val sentence1 = Sentence(
    IndexedSeq(Token('the), Token('tigers), Token('roar), Token('and), Token('run))
  )

  "Sentence.taggedWithVerbnetClasses" should "return the correct answer" in {
    val verbnetTagger = VerbnetTagger(verbnet)
    val postaggerTransform = PolyPostaggerSentenceTransform(FactoriePostaggerInitializer(
      useCoarseTags = false
    ))
    verbnetTagger.transform(postaggerTransform.transform(sentence1)) shouldBe
      Sentence(IndexedSeq(
        Token('nexxx, Map(
          'verbnetPrimaryFrames -> Set()
        )),
        Token('tigers, Map(
          'autoPos -> Set('NNS),
          'autoCpos -> Set('NOUN),
          'factorieLemma -> Set('tiger),
          'verbnetPrimaryFrames -> Set()
        )),
        Token('roar, Map(
          'autoPos -> Set('NN),
          'autoCpos -> Set('NOUN),
          'factorieLemma -> Set('roar),
          'verbnetPrimaryFrames ->
            Set(
              Symbol("NP-V-PP"), Symbol("NP-V-PP-how-S_INF"),
              Symbol("NP-V-S_INF"), Symbol("NP-V-NP-PP"), Symbol("NP-V-PP"),
              Symbol("PP-V-NP"), Symbol("NP-V-NP"), Symbol("NP-V-PP-S_INF"),
              Symbol("NP-V-S-Quote"), Symbol("It-V-PP"), Symbol("NP-V-that-S"),
              Symbol("NP-V-PP-that-S"), Symbol("NP-V"), Symbol("NP-V-PP"),
              Symbol("NP-V-PP"), Symbol("NP-V-PP"),
              Symbol("NP-V-PP-S-Quote"), Symbol("There-V-NP-PP"), Symbol("It-V"),
              Symbol("NP-V-how-S_INF"), Symbol("It-V-NP"), Symbol("There-V-PP-NP"),
              Symbol("NP-V-NP")
            )
        )),
        Token('and, Map(
          'autoPos -> Set('CC),
          'autoCpos -> Set('CONJ),
          'factorieLemma -> Set('and),
          'verbnetPrimaryFrames -> Set()
        )),
        Token('run, Map(
          'autoPos -> Set('VB),
          'autoCpos -> Set('VERB),
          'factorieLemma -> Set('run),
          'verbnetPrimaryFrames ->
            Set(
              Symbol("NP-V-PP"), Symbol("NP-V-NP-PP-PP"),
              Symbol("PP-V-NP"), Symbol("NP-V-NP-PP"),
              Symbol("NP-V-NP-PP"), Symbol("NP-V-NP-PP-PP"),
              Symbol("NP-V-NP-NP"), Symbol("There-V-NP-PP"),
              Symbol("PP-V-PP"), Symbol("NP-V-NP-PP"),
              Symbol("There-V-PP-NP"), Symbol("NP-V-NP")
            )
        ))
      ))
  }
}
