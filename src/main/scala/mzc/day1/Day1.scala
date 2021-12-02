package mzc.day1

import scala.io.Source

object Day1 extends App {
  def partOne(input: List[Int]): Int =
    input
      .sliding(2)
      .map(pair => pair(1) > pair(0))
      .count(identity)

  def partTwo(input: List[Int]): Int = {
    input
      .sliding(4)
      .map { firstWindow =>
        val windowSums = firstWindow.sliding(3).map(_.sum).toList
        windowSums(1) > windowSums(0)
      }
      .count(identity)
  }

  val input = Source.fromResource("day1/input.txt").getLines.map(_.toInt).toList

  println(partOne(input))
  println(partTwo(input))
}
