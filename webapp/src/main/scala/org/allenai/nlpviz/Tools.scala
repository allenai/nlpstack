package org.allenai.nlpviz

sealed abstract class Tool(val name: String, val shortName: String) {
  require(name matches "[a-zA-Z]+")
  require(shortName matches "[a-zA-Z]+")

  val names = Seq(name, shortName)
}
case object Dependencies extends Tool("dependencies", "deps")