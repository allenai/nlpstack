package org.allenai.nlpstack
package chunk

import org.allenai.nlpstack.tokenize._
import org.allenai.nlpstack.postag._

import org.allenai.common.testkit.UnitSpec

class TokenizerImplSpec extends UnitSpec {
  "tokenizer implementation" should "correctly tokenize two example sentences" in {
    val text =
      """|The battle station is heavily shielded and carries a firepower greater
         |than half the star fleet. Its defenses are designed around a direct,
         |large-scale assault. A small one-man fighter should be able to
         |penetrate the outer defense.""".stripMargin

    val tokenizer = new SimpleEnglishTokenizer
    val tokenized = tokenizer.tokenize(text)
    val tokenizedString = tokenized.mkString("; ");
    assert(tokenizedString ===
        "The 0; battle 4; station 11; is 19; heavily 22; shielded 30; and 39; carries 43; a 51; firepower 53; greater 63; than 71; half 76; the 81; star 85; fleet 90; . 95; Its 97; defenses 101; are 110; designed 114; around 123; a 130; direct 132; , 138; large-scale 140; assault 152; . 159; A 161; small 163; one-man 169; fighter 177; should 185; be 192; able 195; to 200; penetrate 203; the 213; outer 217; defense 223; . 230")
  }
}

