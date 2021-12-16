package mzc.template

import scala.io.Source

object DayN extends App {
  type Input = List[String]

  def parseInput(): Input = {
    Source
      .fromResource("dayN/input.txt")
      .getLines
      .toList
  }

  def partOne(input: Input): Int = {
    1
  }

  def partTwo(input: Input): Int = {
    2
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}