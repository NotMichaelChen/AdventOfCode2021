package mzc.day11

import mzc.util.Matrix

import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {
  def nextStep(energyMap: Matrix[Int]): Matrix[Int] = {
    val incrementedMap = energyMap.map(_ + 1)

    @tailrec
    def loop(energyMap: Matrix[Int]): Matrix[Int] = {
      val nextEnergyMap =
        energyMap.mapIndices { point =>
          val energyValue = energyMap.get(point)

          if (energyValue == 0 || energyValue > 9)
            0
          else {
            val adjacentPoints = energyMap.adjacentIndices(point)

            val incBy = adjacentPoints.count(p => energyMap.get(p) > 9)
            energyValue + incBy
          }
        }

      if (nextEnergyMap.exists(_ > 9))
        loop(nextEnergyMap)
      else
        nextEnergyMap
    }

    loop(incrementedMap)
  }

  type Input = Vector[Vector[Int]]

  def parseInput(): Input = {
    Source
      .fromResource("day11/input.txt")
      .getLines
      .toVector
      .map(_.map(_.asDigit).toVector)
  }

  def partOne(input: Input): Int = {
    val energyMap = Matrix(input)

    def loop(energyMap: Matrix[Int], flashes: Int, generation: Int): Int = {
      if (generation == 100) {
        flashes
      } else {
        val nextEnergyMap = nextStep(energyMap)
        val newFlashes = nextEnergyMap.count(_ == 0)
        loop(nextEnergyMap, flashes + newFlashes, generation + 1)
      }
    }

    loop(energyMap, 0, 0)
  }

  def partTwo(input: Input): Int = {
    val energyMap = Matrix(input)

    def loop(energyMap: Matrix[Int], generation: Int): Int = {
      if (energyMap.matrix.forall(_.forall(_ == 0)))
        generation
      else {
        val nextEnergyMap = nextStep(energyMap)
        loop(nextEnergyMap, generation + 1)
      }
    }

    loop(energyMap, 0)
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}
