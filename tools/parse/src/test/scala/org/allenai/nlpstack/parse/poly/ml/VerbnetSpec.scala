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
          'lcase -> Set('nexus),
          'cpos -> Set('nexus),
          'verbnetPrimaryFrames -> Set(),
          'verbnetSecondaryFrames -> Set()
        )),
        Token('tigers, Map(
          'factoriePos -> Set('NNS),
          'factorieCpos -> Set('NOUN),
          'factorieLemma -> Set('tiger),
          'verbnetPrimaryFrames -> Set(),
          'verbnetSecondaryFrames -> Set()
        )),
        Token('roar, Map(
          'factoriePos -> Set('NN),
          'factorieCpos -> Set('NOUN),
          'factorieLemma -> Set('roar),
          'verbnetPrimaryFrames ->
            Set(
              Symbol("NP V PP.location"), Symbol("NP V PP.recipient how S_INF"),
              Symbol("NP V S_INF"), Symbol("NP V NP PP.recipient"), Symbol("NP V PP.recipient"),
              Symbol("PP.location V NP"), Symbol("NP V NP.theme"), Symbol("NP V PP.recipient S_INF"),
              Symbol("NP V S-Quote"), Symbol("It V PP.theme"), Symbol("NP V that S"),
              Symbol("NP V PP.recipient that S"), Symbol("NP V"), Symbol("NP.location V PP.theme"),
              Symbol("NP.location V PP.agent"), Symbol("NP V PP.topic"),
              Symbol("NP V PP.recipient S-Quote"), Symbol("There V NP PP"), Symbol("It V"),
              Symbol("NP V how S_INF"), Symbol("It V NP.theme"), Symbol("There V PP NP"),
              Symbol("NP V NP")
            ),
          'verbnetSecondaryFrames ->
            Set(
              Symbol("NP; Expletive Subject, Theme Object"), Symbol("NP; Theme Object"),
              Symbol("PP; Location Subject, with-PP"), Symbol("PP; Topic-PP"),
              Symbol("PP-QUOT; Recipient-PP"), Symbol("QUOT"),
              Symbol("NP-PP; Expletive-there Subject"), Symbol("PP-HOW-TO-INF; Recipient-PP"),
              Symbol("PP; over-PP"), Symbol("NP-PP; Recipient-PP"), Symbol("There-insertion"),
              Symbol("HOW-TO-INF"), Symbol("PP-TO-INF-OC; Recipient-PP"),
              Symbol("PP-NP; Expletive-there Subject"), Symbol("PP; Location-PP"),
              Symbol("Locative Inversion"), Symbol("PP; Expletive Subject, Theme-PP"),
              Symbol("Intransitive; Expletive Subject"), Symbol("Basic Intransitive; inchoative"),
              Symbol("NP; Causative variant"), Symbol("PP-S; Recipient-PP"),
              Symbol("Basic Intransitive"), Symbol("TO-INF-AC"), Symbol("PP; Recipient-PP"),
              Symbol("S"), Symbol("Basic Transitive")
            )
        )),
        Token('and, Map(
          'factoriePos -> Set('CC),
          'factorieCpos -> Set('CONJ),
          'factorieLemma -> Set('and),
          'verbnetPrimaryFrames -> Set(),
          'verbnetSecondaryFrames -> Set()
        )),
        Token('run, Map(
          'factoriePos -> Set('VB),
          'factorieCpos -> Set('VERB),
          'factorieLemma -> Set('run),
          'verbnetPrimaryFrames ->
            Set(
              Symbol("NP V PP.location"), Symbol("NP V NP PP.destination PP.initial_location"),
              Symbol("PP.location V NP"), Symbol("NP V NP PP.destination"),
              Symbol("NP V NP PP.beneficiary"), Symbol("NP V NP PP.initial_location PP.destination"),
              Symbol("NP V NP.beneficiary NP"), Symbol("There V NP PP"),
              Symbol("PP.location V PP.theme"), Symbol("NP V NP PP.initial_location"),
              Symbol("There V PP NP"), Symbol("NP V NP")
            ),
          'verbnetSecondaryFrames ->
            Set(
              Symbol("NP-PP; Goal-PP"), Symbol("Basic Transitive; with accompanied motion"),
              Symbol("PP; Location Subject, with-PP"), Symbol("NP-PP; Initial_Location-PP"),
              Symbol("NP-PP; Expletive-there Subject"),
              Symbol("NP-PP; for-PP"), Symbol("NP-NP; Beneficiary Object"),
              Symbol("Locative Preposition Drop"), Symbol("NP-PP-PP; Initial_Location-PP Goal-PP"),
              Symbol("PP; against-PP"), Symbol("PP-NP; Expletive-there Subject"),
              Symbol("NP-PP-PP; Goal-PP Initial_Location-PP"), Symbol("Locative Inversion"),
              Symbol("PP; locative-PP"),
              Symbol("Basic Transitive"), Symbol("PP; path-PP")
            )
        ))
      ))
  }
}
