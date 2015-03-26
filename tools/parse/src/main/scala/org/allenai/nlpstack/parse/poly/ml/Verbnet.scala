package org.allenai.nlpstack.parse.poly.ml

import edu.mit.jverbnet.data._
import edu.mit.jverbnet.index._

import scala.collection.JavaConversions._

import java.net._

import spray.json._
import DefaultJsonProtocol._

/** A class that uses JVerbnet, a 3rd party Wrapper library for Verbnet data
  * (http://projects.csail.mit.edu/jverbnet/),  to quickly look up various vernbet
  * features for a verb.
  */
case class Verbnet(verbnetPath: String) {

  // Construct the index and open it
  @transient val index = {
    val url = new URL("file", null, verbnetPath)
    val ix = new VerbIndex(url)
    ix.open
    ix
  }

  // Construct a table mapping a word (verb) to the set of verbnet classes it can be part of.
  @transient private val verbnetClassTable = {
    val table = scala.collection.mutable.HashMap.empty[Symbol, Set[IVerbClass]]
    // Iterate through verb classes, and for each, extract the verb and
    // its synonyms. Populate synonym map.
    val vList = index.iterator.toList
    for (vClass <- vList) {
      for (classMember <- vClass.getMembers) {
        val member = Symbol(classMember.getName.trim)
        if (!table.containsKey(member)) {
          table.put(member, Set.empty[IVerbClass] + vClass)
        } else {
          table.update(member, table(member) + vClass)
        }
      }
    }
    table.toMap
  }

  /* Returns the set of  all classes for a given verb.
  .*/
  def getVerbnetClasses(verb: String): Set[IVerbClass] = {
    val verbSym = Symbol(verb)
    verbnetClassTable.getOrElse(verbSym, Set.empty[IVerbClass])
  }

  /* Returns the names of all classes for a given verb.
  .*/
  def getVerbnetClassNames(verb: String): Set[Symbol] = {
    val verbClasses = getVerbnetClasses(verb)
    verbClasses.map(x => Symbol(x.getID))
  }

  /* Returns the set of all frames within all classes for a given verb.
   */
  def getVerbnetFrames(verb: String): Set[IFrame] = {
    // Get all classes
    val verbClasses = getVerbnetClasses(verb)
    verbClasses flatMap { verbClass => verbClass.getFrames }
  }

  /* Returns the set of primary names for all frames within all classes
   * for a given verb.
   */
  def getVerbnetFramePrimaryNames(verb: String): Set[Symbol] = {
    // Get frames
    val verbFrames = getVerbnetFrames(verb)
    // Split the frame name into its constituents, for e.g., "NP", "V" and "PP.result" from
    // the fram name "NP V PP.result". Then strip the part following a dot, if it exists, from
    // each of the constituents. For e.g., "PP.result" becomes "PP", so the entire frame name
    // will become "NP V PP". Then replace whitespaces with dashes, to get "NP-V-PP".
    val frames = (for {
      verbFrame <- verbFrames
    } yield {
      val primaryName = verbFrame.getPrimaryType.getID
      val constituents = primaryName.split("""\s+""")
      val modConstituents = for {
        constituent <- constituents
      } yield {
        val pattern = """^(.+)\..+$""".r
        val patternMatch = pattern.findFirstMatchIn(constituent)
        patternMatch match {
          case Some(x) => x.group(1)
          case None => constituent
        }
      }
      modConstituents.mkString("-")
    }).map(x => Symbol(x)).toSet
    frames
  }

  /* Returns the set of secondary names for all frames within all classes
   * for a given verb.
   */
  def getVerbnetFrameSecondaryNames(verb: String): Set[Symbol] = {
    // Get frames
    val verbFrames = getVerbnetFrames(verb)
    verbFrames.map(_.getSecondaryType).filter(secondaryType => secondaryType != null).map(
      secondaryType => Symbol(secondaryType.getID.replaceAll("""\s+""", "-"))
    )
  }
}

object Verbnet {

  implicit val jsonFormat = jsonFormat1(Verbnet.apply)

  val commonPrimaryFrames: Set[Symbol] = (Seq(
    "NP V NP ADJP",
    "NP V for NP S_INF",
    "NP V PP.recipient",
    "NP V PP.recipient S_INF",
    "NP V PP.destination",
    "NP V S-Quote",
    "NP V NP.recipient",
    "NP V NP.stimulus",
    "NP V PP.recipient that S",
    "NP.patient V",
    "NP.location V NP",
    "NP V how S",
    "NP V ADV",
    "NP V NP.patient",
    "NP V NP-Dative NP",
    "NP V what S_INF",
    "NP V what S",
    "NP V PP.recipient S-Quote",
    "NP V NP PP.source NP.asset",
    "NP V NP PP.material",
    "NP V S_INF",
    "NP V NP S_INF",
    "NP V NP.destination",
    "NP V NP PP.asset",
    "NP.instrument V NP",
    "NP V PP.topic",
    "NP V how S_INF",
    "NP V ADV-Middle",
    "There V NP PP",
    "NP.asset V NP",
    "There V PP NP",
    "NP V NP PP.theme",
    "NP V NP ADJ",
    "NP V NP.theme",
    "NP V S_ING",
    "NP V NP PP.recipient",
    "NP V NP PP.location",
    "NP V NP PP.destination",
    "NP V NP PP.source",
    "PP.location V NP",
    "NP V NP S_ING",
    "NP V NP PP.instrument",
    "NP V NP.beneficiary NP",
    "NP V NP PP.attribute",
    "NP V NP PP.beneficiary",
    "NP V NP to be NP",
    "NP V NP PP.result",
    "NP V PP.location",
    "NP V NP NP",
    "NP V that S",
    "NP V",
    "NP V PP.attribute",
    "NP V NP"
  ) map { Symbol(_) }).toSet

  val commonSecondaryFrames: Set[Symbol] = (Seq(
    "P-WH-TO-INF",
    "NP-ADVP-PRED",
    "PP-TO-INF-OC",
    "NP-WHAT-TO-INF",
    "NP-WHAT-S",
    "P-WH-S",
    "NP-HOW-S",
    "on-PP",
    "Recipient-PP Theme-PP",
    "WH-S",
    "Attribute Object",
    "Source-PP",
    "inchoative",
    "NP-P-ING-AC",
    "P-WHAT-TO-INF",
    "P-WHAT-S",
    "Initial_Location-PP Goal-PP",
    "PP-HOW-TO-INF",
    "P-POSSING",
    "NP-VEN-NP-OMIT",
    "Initial_Location-PP",
    "Locative Preposition Drop",
    "P-ING-SC",
    "NP-P-ING-OC",
    "FOR-TO-INF",
    "PP-PP",
    "Goal-PP",
    "TO-INF-AC",
    "PP-QUOT",
    "NP-TOBE",
    "NP-S",
    "Location Subject Alternation",
    "QUOT",
    "path-PP",
    "in-PP",
    "NP-NP-PP",
    "Simple Reciprocal Intransitive",
    "Topic-PP",
    "Recipient Object",
    "ING-SC/BE-ING",
    "Possessor Object, Attribute-PP",
    "Attribute Object, Possessor-PP",
    "Plural Subject",
    "HOW-S",
    "ADVP-PRED",
    "NP-TO-INF-OC",
    "Location Subject, with-PP",
    "Dative",
    "POSSING",
    "WHAT-TO-INF",
    "about-PP",
    "Asset-PP",
    "Instrument Subject Alternation",
    "Destination-PP",
    "WHAT-S",
    "PP-S",
    "double object",
    "Source-PP, Asset-PP",
    "Benefactive Alternation",
    "Material-PP",
    "Inchoative",
    "S-SUBJUNCT",
    "Theme-PP",
    "Causative variant",
    "Causative",
    "Result-PP",
    "Destination Object",
    "NP-ADJP",
    "Resultative",
    "Unspecified Object",
    "here/there",
    "HOW-TO-INF",
    "Instrument-PP",
    "Theme Object",
    "Middle Construction",
    "to-PP",
    "Asset Subject",
    "Recipient-PP",
    "from-PP",
    "PP-NP",
    "with-PP",
    "Locative Inversion",
    "Beneficiary Object",
    "locative-PP",
    "for-PP",
    "Expletive-there Subject",
    "Intransitive",
    "Infinitival Copular Clause",
    "Location-PP",
    "NP-PP-PP",
    "S",
    "as-PP",
    "Basic Intransitive",
    "Transitive",
    "NP-NP",
    "NP",
    "PP",
    "Basic Transitive",
    "PP-PRED-RSin-PP",
    "NP-PP"
  ) map { Symbol(_) }).toSet
}
