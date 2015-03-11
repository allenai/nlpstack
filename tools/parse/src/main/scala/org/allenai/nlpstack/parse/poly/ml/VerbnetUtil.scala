package org.allenai.nlpstack.parse.poly.ml

import org.allenai.datastore._

import com.typesafe.config.{ Config, ConfigFactory }

import edu.mit.jverbnet.data._
import edu.mit.jverbnet.index._

import scala.collection.JavaConversions._

import java.io.{ File, FileWriter, Writer }
import java.net._

/** An object that uses JVerbnet, a 3rd party Wrapper library for Verbnet data
  * (http://projects.csail.mit.edu/jverbnet/),  to quickly look up vernbet classes
  * for a given symbol.
  */
object VerbnetUtil {

  /* Returns a table mapping a word (verb) to the set of verbnet classes it can be part of.
   * @verbnetConfigFilePath path to a config file containing datastore location info to access the 
   * verbnet resource.
  .*/
  def getVerbnetClassMap(verbnetConfigFilePath: String): Map[Symbol, Set[Symbol]] = {
    // Get Verbnet local path.
    val config = ConfigFactory.parseFile(new File(verbnetConfigFilePath))

    val verbnetConfig = config.getConfig("verbnet")
    val groupName = verbnetConfig.getString("group")
    val artifactName = verbnetConfig.getString("name")
    val version = verbnetConfig.getInt("version")

    val path: java.nio.file.Path = Datastore.directoryPath(
      groupName,
      artifactName,
      version
    )

    val verbnetClassTable: scala.collection.mutable.Map[Symbol, Set[Symbol]] =
      scala.collection.mutable.HashMap.empty[Symbol, Set[Symbol]]

    // Read Verbnet files from given path, construct map.    
    // Create and open Verbnet index object for querying.
    val url = new URL("file", null, path.toString)
    // Construct the index and open it
    val index = new VerbIndex(url)
    index.open

    // Iterate through verb classes, and for each, extract the verb and
    // its synonyms. Populate synonym map.
    val vList = index.iterator.toList
    for (vClass <- vList) {

      val vId = vClass.getID
      val vIdSym = Symbol(vId.trim)

      for (classMember <- vClass.getMembers) {
        val member = Symbol(classMember.getName.trim)
        if (!verbnetClassTable.containsKey(member)) {
          verbnetClassTable.put(member, Set.empty[Symbol] + vIdSym)
        } else {
          verbnetClassTable.update(member, verbnetClassTable(member) + vIdSym)
        }
      }
    }

    verbnetClassTable.toMap
  }

}