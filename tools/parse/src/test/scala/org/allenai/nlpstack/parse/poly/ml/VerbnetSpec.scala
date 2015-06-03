package org.allenai.nlpstack.parse.poly.ml

import org.allenai.common.Logging
import org.allenai.common.testkit.UnitSpec

import com.typesafe.config.ConfigFactory
import java.io.File

import org.allenai.nlpstack.parse.poly.core.SentenceTagger

class VerbnetSpec extends UnitSpec with Logging {

  val taggersConfig = ConfigFactory.parseFile(new File(SentenceTagger.taggersConfigFile))
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

}
