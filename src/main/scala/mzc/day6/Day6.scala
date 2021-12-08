package mzc.day6

import scala.annotation.tailrec
import scala.io.Source

object Day6 extends App {
  def parseInput(): List[Int] = {
    Source
      .fromResource("day6/input.txt")
      .getLines
      .toList
      .flatMap(_.split(',').map(_.toInt))
  }

  def simulateFish(fishList: List[Int], generations: Int): Long = {
    @tailrec
    def simulate(fishBuckets: Array[Long], day: Int): Long = {
      val newFish = fishBuckets(0)
      val rotatedBuckets = fishBuckets.tail :+ newFish
      val updatedBuckets = rotatedBuckets.updated(6, rotatedBuckets(6) + fishBuckets(0))

      if (day == generations)
        updatedBuckets.sum
      else
        simulate(updatedBuckets, day + 1)
    }

    val initBuckets = fishList.foldLeft(Array.fill(9)(0L))((buckets, fishLife) => buckets.updated(fishLife, buckets(fishLife) + 1))

    simulate(initBuckets, 1)
  }

  def partOne(input: List[Int]): Long = {
    simulateFish(input, 80)
  }

  def partTwo(input: List[Int]): Long = {
    simulateFish(input, 256)
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}