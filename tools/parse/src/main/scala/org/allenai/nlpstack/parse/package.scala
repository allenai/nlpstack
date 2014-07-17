package org.allenai.nlpstack

import org.allenai.nlpstack.core.DependencyParser

package object parse {
  val defaultDependencyParser: DependencyParser = new FactorieParser
}
