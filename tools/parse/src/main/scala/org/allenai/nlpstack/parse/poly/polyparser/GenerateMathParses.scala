package org.allenai.nlpstack.parse.poly.polyparser

import java.io.{ FileWriter, BufferedWriter, PrintWriter }

import org.allenai.common.Resource

import scala.util.Random

object GenerateMathParses {

  def writeToFile(numTrees: Int, filename: String): Unit = {
    Resource.using(new PrintWriter(new BufferedWriter(new FileWriter(filename)))) { writer =>
      Range(0, numTrees) foreach { treeIndex =>
        writer.write(generateEquationTree(0.3).asConllX + "\n\n")
      }
    }
  }

  private def chooseFrom[A](elements: Seq[A]): A = {
    elements(Random.nextInt(elements.size))
  }

  def generateModifier(): PolytreeParse = {
    val nextTemplateStr = chooseFrom(Seq(
      ("In|IN the|DT diagram|NN above|JJ ,|,", "0|PREP 3|DET 1|POBJ 3|AMOD 1|PUNCT"),
      ("In|IN the|DT attached|JJ diagram|NN ,|,", "0|PREP 4|DET 4|AMOD 1|POBJ 1|PUNCT"),
      ("In|IN the|DT figure|NN below|JJ ,|,", "0|PREP 3|DET 1|POBJ 3|AMOD 1|PUNCT"),
      ("", "")
    ))
    ParseSubstitution.constructVariableParse(nextTemplateStr._1, nextTemplateStr._2)
  }

  def generateEquationTree(conjunctionProb: Double): PolytreeParse = {
    val expressionType = Random.nextDouble()
    if (expressionType < conjunctionProb) {
      val nextTemplateStr = chooseFrom(Seq(
        ("$0 $1 =|SYM $2 and|CC $3 .|.", "3|PREP 3|NARG 0|ROOT 3|NARG 3|CC 3|CONJ 3|PUNCT"),
        ("$0 $1 =|SYM $2 ,|, $3 ,|, and|CC $4 .|.", "3|PREP 3|NARG 0|ROOT 3|NARG 3|PUNCT 3|CONJ 3|PUNCT 3|CC 3|CONJ 3|PUNCT")
      ))
      val nextTemplate = ParseSubstitution.constructVariableParse(nextTemplateStr._1, nextTemplateStr._2)
      ParseSubstitution.substitute(nextTemplate, Map(
        0 -> generateModifier(),
        1 -> generateNumericTree(Seq(1, 1)),
        2 -> generateNumericTree(Seq(0.7, 1)),
        3 -> generateSimpleEquationTree(),
        4 -> generateSimpleEquationTree()
      ))
    } else {
      val templates = Seq(
        ("$0 $1 =|SYM $2 .|.", "3|PREP 3|NARG 0|ROOT 3|NARG 3|PUNCT"),
        ("$0 $1 is|VBZ equal|JJ to|TO $2 .|.", "3|PREP 3|NARG 0|ROOT 3|ACOMP 4|PREP 5|NARG 3|PUNCT")
      ) map { template => ParseSubstitution.constructVariableParse(template._1, template._2) }
      val nextTemplate = templates(Random.nextInt(templates.size))
      ParseSubstitution.substitute(nextTemplate, Map(
        0 -> generateModifier(),
        1 -> generateNumericTree(Seq(1, 1)),
        2 -> generateNumericTree(Seq(0.7, 1))
      ))
    }
  }

  def generateSimpleEquationTree(): PolytreeParse = {
    val templates = Seq(
      ("$1 =|SYM $2", "2|NARG 0|ROOT 2|NARG"),
      ("$1 is|VBZ equal|JJ to|TO $2", "2|NARG 0|ROOT 2|ACOMP 3|PREP 4|NARG")
    ) map { template => ParseSubstitution.constructVariableParse(template._1, template._2) }
    val nextTemplate = templates(Random.nextInt(templates.size))
    ParseSubstitution.substitute(nextTemplate, Map(
      1 -> generateNumericTree(Seq(1, 1)),
      2 -> generateNumericTree(Seq(0.7, 1))
    ))
  }

  def generateNumericTree(intervals: Seq[Double]): PolytreeParse = {
    val numericTemplates = Seq(
      ("$0 +|SYM $1", "2|NARG 0|ROOT 2|NARG"),
      ("$0 -|SYM $1", "2|NARG 0|ROOT 2|NARG"),
      ("$0 *|SYM $1", "2|NARG 0|ROOT 2|NARG"),
      ("$0 /|SYM $1", "2|NARG 0|ROOT 2|NARG"),
      ("$0 ^|SYM $1", "2|NARG 0|ROOT 2|NARG")
    ) map { template => ParseSubstitution.constructVariableParse(template._1, template._2) }
    val expressionType = Random.nextDouble()
    val areaTemplates = Seq(
      ("the|DT $0 of|IN $1", "2|DET 0|ROOT 2|PREP 3|POBJ")
    )
    if (expressionType < intervals(0)) {
      val nextTemplate = numericTemplates(Random.nextInt(numericTemplates.size))
      ParseSubstitution.substitute(nextTemplate, Map(
        0 -> generateNumericTree(Seq(0.0, 0.0)),
        1 -> generateNumericTree(Seq(0.0, 0.0))
      ))
    } else if (expressionType < intervals(1)) {
      val nextTemplateStr = chooseFrom(areaTemplates)
      val nextTemplate = ParseSubstitution.constructVariableParse(nextTemplateStr._1, nextTemplateStr._2)
      ParseSubstitution.substitute(nextTemplate, Map(
        0 -> generateAreaWord(),
        1 -> generateNumericLeaf(0)
      ))
    } else {
      generateNumericLeaf(0.5)
    }
  }

  def generateAreaWord(): PolytreeParse = {
    val randomVar = chooseFrom(Seq("area", "perimeter", "length"))
    ParseSubstitution.constructVariableParse(s"$randomVar|NN", "0|ROOT")
  }

  def generateNumericLeaf(numberProb: Double): PolytreeParse = {
    def getShapeFromVariable(variableName: String): String = {
      variableName.size match {
        case 1 => chooseFrom(Seq("point", "circle"))
        case 2 => chooseFrom(Seq("line", "arc"))
        case 3 => chooseFrom(Seq("angle", "triangle"))
        case 4 => chooseFrom(Seq("square", "rectangle", "parallelogram", "rhombus", "trapezoid"))
        case _ => "polygon"
      }
    }
    val expressionType = Random.nextDouble()
    if (expressionType < 0.6 * numberProb) {
      val randomInt = Random.nextInt(100)
      ParseSubstitution.constructVariableParse(s"$randomInt|CD", "0|ROOT")
    } else if (expressionType < numberProb) {
      val randomDouble = (Random.nextDouble() * 10000).toInt / 100.0
      ParseSubstitution.constructVariableParse(s"$randomDouble|CD", "0|ROOT")
    } else if (expressionType < 0.75) {
      val singleVars = Vector("x", "y", "z", "a", "b")
      val randomVar = singleVars(Random.nextInt(singleVars.size))
      ParseSubstitution.constructVariableParse(s"$randomVar|SYM", "0|ROOT")
    } else {
      val geometryVars = "ABCDEF"
      val randomSubset = Range(0, geometryVars.size) map { x =>
        (geometryVars(x), Random.nextDouble() < 0.4)
      } filter {
        case (_, keep) =>
          keep
      } map {
        case (geovar, _) =>
          geovar
      }
      val randomVar =
        if (randomSubset.size == 0) {
          "A"
        } else {
          randomSubset.mkString("")
        }
      if (expressionType < 0.85) {
        ParseSubstitution.constructVariableParse(s"$randomVar|SYM", "0|ROOT")
      } else {
        val shape = getShapeFromVariable(randomVar)
        ParseSubstitution.constructVariableParse(s"$shape|JJ $randomVar|SYM", "2|AMOD 0|ROOT")
      }
    }
  }
}
