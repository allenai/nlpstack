package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser.{
  PolytreeParse,
  PolytreeParseSource,
  PolytreeParseFileFormat,
  ConllX,
  FileBasedPolytreeParseSource
}

/** A ParseBank maps string-representations of sentences to a unique parse.
  *
  * @param underlyingMap the map from strings to parses
  */
case class ParseBank(underlyingMap: Map[String, PolytreeParse]) {

  /** Takes a candidate parse tree and finds the parse in the bank corresponding to its
    * sentence. If no parse exists (or if the existing parse has a different tokenization than
    * the candidate parse), then None is returned.
    *
    * @param candParse the parse, for which we want to find a corresponding banked parse
    * @return the corresponding banked parse, if it exists
    */
  def askForCorrespondingGoldParse(candParse: PolytreeParse): Option[PolytreeParse] = {
    underlyingMap.get(candParse.sentence.asWhitespaceSeparatedString) match {
      case Some(goldParse) =>
        if ((candParse.tokens map { tok => tok.word }) != (goldParse.tokens map { tok => tok.word })) {
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

  /** Creates a parse bank from a source of parse trees.
    *
    * If the parse source contains multiple parses for the same sentence, then the final parse
    * is used.
    *
    * @param parseSource a source of parse trees
    * @return the corresponding parse bank
    */
  def createParseBankFromSource(parseSource: PolytreeParseSource): ParseBank = {
    ParseBank((parseSource.parseIterator map { parse =>
      (parse.sentence.asWhitespaceSeparatedString, parse)
    }).toMap)
  }

  /** Creates a parse bank from a file containing parse trees
    *
    * If the file contains multiple parses for the same sentence, then the final parse
    * is used.
    *
    * @param filename the file containing the parse trees
    * @param fileFormat the file format (defaults to ConllX)
    * @return the corresponding parse bank
    */
  def createParseBankFromFile(
    filename: String,
    fileFormat: PolytreeParseFileFormat = ConllX(true)
  ): ParseBank = {

    createParseBankFromSource(FileBasedPolytreeParseSource(filename, fileFormat))
  }
}
