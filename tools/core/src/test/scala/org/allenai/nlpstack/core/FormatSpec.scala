package org.allenai.nlpstack.core

import org.allenai.common.testkit.UnitSpec
import org.allenai.nlpstack.core.Format.Quoter

class FormatSpec extends UnitSpec {
  "stringQuoter" should "quote strings" in {
    assert(Format.stringQuoter.quote("A 3\" diameter") === "A 3\\\" diameter")
    assert(Format.stringQuoter.quote("C:\\Windows\\System32") === "C:\\\\Windows\\\\System32")
  }

  "custom Quoter" should "quote strings" in {
    val q = new Quoter(";\"")
    val unquoted = "To be; Or \\not\\ \"to be\""
    val quoted = "To be\\; Or \\\\not\\\\ \\\"to be\\\""
    assert(q.quote(unquoted) == quoted)
    assert(q.unquote(quoted) == unquoted)
  }
}
