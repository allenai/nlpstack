package org.allenai.aitk
package parse

abstract class ConstituencyParserMain
    extends LineProcessor("constit-parser") {
  def constituencyParser: ConstituencyParser
  override def process(line: String) = {
    constituencyParser.parse(line).toString
  }
}
