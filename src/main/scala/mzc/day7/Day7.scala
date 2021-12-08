package mzc.day7

import scala.io.Source

object Day7 extends App {
  def triangleNumber(n: Long): Long = (n * (n - 1)) / 2

  def parseInput(): List[Int] = {
    Source
      .fromResource("day7/input.txt")
      .getLines
      .toList
      .flatMap(_.split(',').map(_.toInt))
  }

  def partOne(input: List[Int]): Int = {
    // just the median?...
    val line = input.sorted.apply(input.length / 2)
    val totalFuel = input.map(pos => Math.abs(pos - line)).sum
    totalFuel
  }

  def partTwo(input: List[Int]): Int = {
    // just the mean?...
    val lineCandidate = input.sum / (input.length * 1.0)
    val upperLine = Math.ceil(lineCandidate).toLong
    val lowerLine = Math.floor(lineCandidate).toLong

    def totalFuel(line: Long): Long = input.map(pos => triangleNumber(Math.abs(pos - line) + 1)).sum

    // ...except we cheat a little?
    Math.min(totalFuel(upperLine), totalFuel(lowerLine)).toInt
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}