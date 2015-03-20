package org.allenai.nlpstack.parse.poly.ml

import edu.mit.jverbnet.data._
import edu.mit.jverbnet.index._

import scala.collection.JavaConversions._

import java.net._
import java.nio.file.Path

import spray.json._
import DefaultJsonProtocol._

/** A class that uses JVerbnet, a 3rd party Wrapper library for Verbnet data
  * (http://projects.csail.mit.edu/jverbnet/),  to quickly look up various vernbet
  * features for a verb.
  */
case class Verbnet(verbnetPath: Path) {

  // Construct the index and open it
  val index = {
    val url = new URL("file", null, verbnetPath.toString)
    val ix = new VerbIndex(url)
    ix.open
    ix
  }

  // Construct a table mapping a word (verb) to the set of verbnet classes it can be part of.
  private val verbnetClassTable = {
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