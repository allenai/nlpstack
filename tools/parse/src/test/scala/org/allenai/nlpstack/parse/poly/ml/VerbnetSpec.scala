package org.allenai.nlpstack.parse.poly.ml

import org.allenai.common.Config.EnhancedConfig
import org.allenai.common.Logging
import org.allenai.common.testkit.UnitSpec
import org.allenai.datastore._
import org.allenai.nlpstack.parse.poly.core.{ FactorieSentenceTagger, VerbnetTagger }
import org.allenai.nlpstack.parse.poly.core.{ Token, Sentence }

import com.typesafe.config.{ Config, ConfigFactory }
import java.io.{ File, FileWriter, Writer }

class VerbnetUtilSpec extends UnitSpec with Logging {

  val taggersConfigPath = "src/main/resources/featuretaggers.config"

  val taggersConfig = ConfigFactory.parseFile(new File(taggersConfigPath))

  val verbnetConfig = taggersConfig.getConfig("verbnet")
  val groupName = verbnetConfig.getString("group")
  val artifactName = verbnetConfig.getString("name")
  val version = verbnetConfig.getInt("version")

  val verbnetPath: java.nio.file.Path = Datastore.directoryPath(
    groupName,
    artifactName,
    version
  )

  val verbnet = new Verbnet(verbnetPath)

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
    verbnetTagger.transform(FactorieSentenceTagger.transform(sentence1)) shouldBe
      Sentence(IndexedSeq(
        Token('nexus, Map(
          'lcase -> Set('nexus), 'cpos -> Set('nexus), 'verbnetClasses -> Set()
        )),
        Token('tigers, Map(
          'factoriePos -> Set('NNS),
          'factorieCpos -> Set('NOUN),
          'factorieLemma -> Set('tiger),
          'verbnetClasses -> Set()
        )),
        Token('roar, Map(
          'factoriePos -> Set('NN),
          'factorieCpos -> Set('NOUN),
          'factorieLemma -> Set('roar),
          'verbnetClasses ->
            Set(
              Symbol("run-51.3.2"),
              Symbol("weather-57"),
              Symbol("animal_sounds-38"),
              Symbol("manner_speaking-37.3"),
              Symbol("sound_emission-43.2")
            )
        )),
        Token('and, Map(
          'factoriePos -> Set('CC),
          'factorieCpos -> Set('CONJ),
          'factorieLemma -> Set('and),
          'verbnetClasses -> Set()
        )),
        Token('run, Map(
          'factoriePos -> Set('VB),
          'factorieCpos -> Set('VERB),
          'factorieLemma -> Set('run),
          'verbnetClasses ->
            Set(
              Symbol("swarm-47.5.1-1"),
              Symbol("meander-47.7"),
              Symbol("carry-11.4"),
              Symbol("preparing-26.3-1"),
              Symbol("run-51.3.2-2-1"),
              Symbol("bump-18.4")
            )
        ))
      ))
  }
}
