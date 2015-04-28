package org.allenai.nlpstack.parse.poly.polyparser

import org.allenai.common.testkit.UnitSpec

class ArcInverterSpec extends UnitSpec {

  "Calling apply" should "give back a modified parse for a simple parse" in {
    val inverter: ArcInverter =
      new ArcInverter(
        Set(SingleSymbolArcLabel('det), SingleSymbolArcLabel('amod), SingleSymbolArcLabel('prep))
      )
    inverter(PolytreeParseTestData.parse1) shouldBe PolytreeParseTestData.parse1b
  }

  it should "give back the same parse with no inverting labels" in {
    val inverter: ArcInverter = new ArcInverter(Set())
    inverter(PolytreeParseTestData.parse1) shouldBe PolytreeParseTestData.parse1
  }

}
