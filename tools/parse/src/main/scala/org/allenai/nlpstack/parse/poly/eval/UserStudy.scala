package org.allenai.nlpstack.parse.poly.eval

import org.allenai.nlpstack.parse.poly.polyparser._
import scopt.OptionParser

private case class UserStudyCommandLine(userFilenames: String = "", goldFilename: String = "")

object UserStudy {

  /** Command-line for evaluating a set of parses against a gold set.
    *
    * Usage: Evaluate [options]
    *
    * -c <file> | --candidate <file>
    * the file containing the candidate parses (CoNLL-X format)
    * -g <file> | --gold <file>
    * the file containing the gold parses (CoNLL-X format)
    *
    * @param args see above
    */
  def main(args: Array[String]) {
    val optionParser = new OptionParser[UserStudyCommandLine]("Evaluate") {
      opt[String]('u', "user") required () valueName ("<file>") action { (x, c) => c.copy(userFilenames = x) } text ("the file containing the candidate " +
        "parses (CoNLL-X format)")
      opt[String]('g', "gold") required () valueName ("<file>") action { (x, c) => c.copy(goldFilename = x) } text ("the file containing the gold " +
        "parses (CoNLL-X format)")
    }
    val clArgs: UserStudyCommandLine = optionParser.parse(args, UserStudyCommandLine()).get

    val userParses = loadUserMap()
    val goldParseBank: ParseBank = ParseBank.createParseBankFromFile(clArgs.goldFilename)

    ParseEvaluation.performStandardMultiUserEvaluation(userParses map { x => x }, goldParseBank)
    val mergedUsers = MultiPolytreeParseSource(userParses.values.toSeq)
    ParseEvaluation.performStandardAnalysis(mergedUsers, goldParseBank)
    ParseEvaluation.performStandardEvaluation(mergedUsers, goldParseBank)

    val stanSource = InMemoryPolytreeParseSource.getParseSource("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/barrons.devandtest.stan.conllx", ConllX(true))
    ParseEvaluation.performStandardEvaluation(stanSource, goldParseBank)
  }

  def loadUserMap(): Map[String, PolytreeParseSource] = {
    Seq(
      ("userA", InMemoryPolytreeParseSource.getParseSource(
        "/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userA.rik.conllx", ConllX(true)
      )),
      ("userB", InMemoryPolytreeParseSource.getParseSource("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userB.tony.conllx", ConllX(true))),
      ("userC", InMemoryPolytreeParseSource.getParseSource("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userC.sumithra.conllx", ConllX(true))),
      ("userD", InMemoryPolytreeParseSource.getParseSource("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userD.sam.conllx", ConllX(true))),
      ("userE", InMemoryPolytreeParseSource.getParseSource("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userE.oyvind.conllx", ConllX(true))),
      ("userF", InMemoryPolytreeParseSource.getParseSource("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userF.kevin.conllx", ConllX(true))),
      ("userG", InMemoryPolytreeParseSource.getParseSource("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userG.cristian.conllx", ConllX(true))),
      ("userH", InMemoryPolytreeParseSource.getParseSource("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userH.chris.conllx", ConllX(true))),
      ("userI", InMemoryPolytreeParseSource.getParseSource("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/userstudy/userI.jesse.conllx", ConllX(true)))
    //("stan", InMemoryPolytreeParseSource.getParseSource("/Users/markhopkins/Projects/data/parsing/treebanks-nonds/barrons/stan3-4-ch-dependencies/newsplit/barrons.devandtest.stan.conllx", ConllX(true)))
    ).toMap
  }
}

