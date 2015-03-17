package org.allenai.nlpstack.parse.poly.ml

import edu.mit.jverbnet.data._
import edu.mit.jverbnet.index._

import scala.collection.JavaConversions._

import java.net._
import java.nio.file.Path

import spray.json._
import DefaultJsonProtocol._

/** An object that uses JVerbnet, a 3rd party Wrapper library for Verbnet data
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
    // Create set of verb frames to populate
    val verbFrames = new scala.collection.mutable.HashSet[IFrame]
    // Iterate through the classes to get all frames within each class.
    for (verbClass <- verbClasses) {
      verbFrames ++= verbClass.getFrames
    }
    verbFrames.toSet
  }

  /* Returns the set of primary names for all frames within all classes 
   * for a given verb. 
   */
  def getVerbnetFramePrimaryNames(verb: String): Set[Symbol] = {
    // Get frames
    val verbFrames = getVerbnetFrames(verb)
    verbFrames.map(x => Symbol(x.getPrimaryType.getID))
  }

  /* Returns the set of secondary names for all frames within all classes 
   * for a given verb. 
   */
  def getVerbnetFrameSecondaryNames(verb: String): Set[Symbol] = {
    // Get frames
    val verbFrames = getVerbnetFrames(verb)
    val secondaryFrameNames = new scala.collection.mutable.HashSet[Symbol]
    for (verbFrame <- verbFrames) {
      val secondaryType = verbFrame.getSecondaryType
      if (secondaryType != null) {
        secondaryFrameNames += Symbol(secondaryType.getID)
      }
    }
    secondaryFrameNames.toSet
  }
}