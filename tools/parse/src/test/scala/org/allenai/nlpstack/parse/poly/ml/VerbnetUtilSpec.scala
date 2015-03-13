package org.allenai.nlpstack.parse.poly.ml

import org.allenai.common.Logging
import org.allenai.common.testkit.UnitSpec
import org.allenai.datastore._
import org.allenai.nlpstack.parse.poly.core.{ FactorieSentenceTagger, VerbnetTagger, Token, Sentence }

class VerbnetUtilSpec extends UnitSpec with Logging {

  val verbnetConfigPath = "src/main/resources/polyparser.config"

  val verbnetClasses = VerbnetUtil.getVerbnetClassMap(verbnetConfigPath)

  "VerbnetUtil.getVerbnetClasses" should
    "return the correct answer for verbs present in VerbNet" in {
      verbnetClasses.get('roar) shouldBe Some(Set(
        Symbol("run-51.3.2"),
        Symbol("weather-57"),
        Symbol("animal_sounds-38"),
        Symbol("manner_speaking-37.3"),
        Symbol("sound_emission-43.2")
      ))
      verbnetClasses.get('boast) shouldBe Some(Set(Symbol("complain-37.8")))
      verbnetClasses.get('synthesize) shouldBe Some(Set(Symbol("create-26.4")))
      verbnetClasses.get('run) shouldBe Some(Set(
        Symbol("swarm-47.5.1-1"),
        Symbol("meander-47.7"),
        Symbol("carry-11.4"),
        Symbol("preparing-26.3-1"),
        Symbol("run-51.3.2-2-1"),
        Symbol("bump-18.4")
      ))
    }

  "VerbnetUtil.getVerbnetClasses" should
    "return the correct answer for words NOT present in VerbNet" in {
      verbnetClasses.get('synthesis) shouldBe None
      verbnetClasses.get('apple) shouldBe None
    }

  val sentence1 = Sentence(
    IndexedSeq(Token('the), Token('tigers), Token('roar), Token('and), Token('run))
  )

  "Sentence.taggedWithVerbnetClasses" should "return the correct answer" in {
    val verbnetTagger = VerbnetTagger(verbnetClasses)
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
