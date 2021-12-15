package mzc.day14

import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {
  def polymerize(template: String, rules: Map[String, String], steps: Int): Map[Char, Long] = {
    // This is needed since we'll count only the first value of each pair which will exclude the last value
    val lastCharacter = template.last

    val pairs = template
      .sliding(2)
      .toVector
      .groupBy(identity).map { case (polymer, occurrences) => (polymer, occurrences.length.toLong) }

    @tailrec
    def pairInsert(pairs: Map[String, Long], rules: Map[String, String], counter: Int): Map[String, Long] = {
      val newPairs =
        pairs.toVector.flatMap { case (pair, count) =>
          val newPairs =
            rules.get(pair) match {
              case Some(element) => pair.patch(1, element, 0).sliding(2).toVector
              case None => Vector(pair)
            }

          newPairs.map(_ -> count)
        }
          .groupMapReduce(_._1)(_._2)(_ + _)

      if (counter == steps)
        newPairs
      else
        pairInsert(newPairs, rules, counter + 1)
    }

    val pairCounts = pairInsert(pairs, rules, 1)
      .groupMapReduce(_._1.head)(_._2)(_ + _)

    pairCounts + (lastCharacter -> (pairCounts(lastCharacter) + 1L))
  }

  type Input = (String, Map[String, String])

  def parseInput(): Input = {
    val rawInput = Source
      .fromResource("day14/input.txt")
      .getLines
      .toList

    val template = rawInput.head
    val rules = rawInput.drop(2).map {
      case s"$pair -> $element" => (pair, element)
    }

    (template, rules.toMap)
  }

  def partOne(input: Input): Long = {
    val (template, rules) = input

    val polymerCounts = polymerize(template, rules, 10)

    val frequencies = polymerCounts.toVector.map(_._2).sorted(Ordering[Long].reverse)

    frequencies.head - frequencies.last
  }

  def partTwo(input: Input): Long = {
    val (template, rules) = input

    val polymerCounts = polymerize(template, rules, 40)

    val frequencies = polymerCounts.toVector.map(_._2).sorted(Ordering[Long].reverse)

    frequencies.head - frequencies.last
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}