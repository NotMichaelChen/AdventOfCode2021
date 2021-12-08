package mzc.template

import scala.io.Source

object DayN extends App {
  def parseInput(): List[String] = {
    Source
      .fromResource("dayN/input.txt")
      .getLines
      .toList
  }

  def partOne(input: List[String]): Int = {
    1
  }

  def partTwo(input: List[String]): Int = {
    2
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}