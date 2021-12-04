package mzc.day3

import scala.annotation.tailrec
import scala.io.Source

object Day3 extends App {
  def xnor(left: Int, right: Int): Int = ~(left ^ right)

  def partOne(input: List[String]): Int = {
    val linecount = input.length

    val gammaBin =
      input
        .map(_.map(_ - '0'))
        .transpose
        .map(_.sum)
        .map(columnSum => if(columnSum >= linecount / 2) "1" else "0")
        .mkString

    val gamma = Integer.parseInt(gammaBin, 2)
    val epsilon = ~gamma & (1 << 12) - 1

    gamma * epsilon
  }

  def partTwo(input: List[String]): Int = {
    @tailrec
    def loop(input: List[String], index: Int, isOxygen: Boolean): Int = {
      val linecount = input.length

      val bitFilter = {
        val onesCount = input.map(line => line(index) - '0').sum
        (onesCount >= (linecount - onesCount)) == isOxygen
      }

      val c = input.filter(line => (line(index) == '1') == bitFilter)

      if (c.length > 1) loop(c, index + 1, isOxygen)
      else Integer.parseInt(c.head, 2)
    }

    val oxygen = loop(input, 0, true)
    val co2 = loop(input, 0, false)

    oxygen * co2
  }

  val input = Source.fromResource("day3/input.txt").getLines.toList

  println(partOne(input))
  println(partTwo(input))
}
