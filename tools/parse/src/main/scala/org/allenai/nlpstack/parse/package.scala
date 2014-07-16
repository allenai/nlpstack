package org.allenai.nlpstack

import org.allenai.nlpstack.core.parse.DependencyParser

package object parse {
  val defaultDependencyParser: DependencyParser = new FactorieParser
}
