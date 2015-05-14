package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser._

case class ParseBank(underlyingMap: Map[String, PolytreeParse]) {

  def askForGoldParse(candParse: PolytreeParse): Option[PolytreeParse] = {
    underlyingMap.get(candParse.sentence.asWhitespaceSeparatedString) match {
      case Some(goldParse) =>
        if (candParse.breadcrumb.size != goldParse.breadcrumb.size ||
          goldParse.breadcrumb.size <= 1) {

          println(s"WARNING -- Skipping parse: ${candParse.sentence.asWhitespaceSeparatedString}" +
            s" tokenized differently than gold: ${goldParse.sentence.asWhitespaceSeparatedString}")
          None
        } else {
          Some(goldParse)
        }
      case None => None
    }
  }
}

object ParseBank {

  def createParseBankFromSource(parseSource: PolytreeParseSource): ParseBank = {
    ParseBank((parseSource.parseIterator map { parse =>
      (parse.sentence.asWhitespaceSeparatedString, parse)
    }).toMap)
  }

  def createParseBankFromFile(
    filename: String,
    fileFormat: PolytreeParseFileFormat = ConllX(true)
  ): ParseBank = {

    createParseBankFromSource(FileBasedPolytreeParseSource(filename, fileFormat))
  }
}
