package mzc.day17

import scala.io.Source

case class Bounds(
                   leftBound: Int,
                   rightBound: Int,
                   bottomBound: Int,
                   topBound: Int
                 )

case class Range(start: Int, end: Option[Int]) {
  def intersects(other: Range): Boolean = {
    this.start <= other.end.getOrElse(Int.MaxValue) &&
      other.start <= this.end.getOrElse(Int.MaxValue)
  }
}

object Range {
  def apply(rawRange: Vector[(Int, Int)]): Range = {
    if (rawRange.length >= 2) {
      val lastTwo = rawRange.takeRight(2)
      if (lastTwo(1)._1 - lastTwo(0)._1 == 1) {
        Range(rawRange.head._2, None)
      } else {
        Range(rawRange.head._2, Some(rawRange.last._2))
      }
    } else {
      Range(rawRange.head._2, Some(rawRange.head._2))
    }
  }
}

object Day17 extends App {
  def inRange(num: Int, lower: Int, upper: Int): Boolean =
    num >= lower && num <= upper

  type Input = Bounds

  def parseInput(): Input = {
    val rawInput =
      Source
        .fromResource("day17/input.txt")
        .getLines
        .toList
        .head

    rawInput match {
      case s"target area: x=$leftBound..$rightBound, y=$bottomBound..$topBound" =>
        Bounds(leftBound.toInt, rightBound.toInt, bottomBound.toInt, topBound.toInt)
    }
  }

  def partOne(input: Input): Int = {
    def simulateY(velocity: Int): Vector[Int] = {
      def loop(position: Int, velocity: Int, points: Vector[Int]): Vector[Int] = {
        if ((position + velocity) < input.bottomBound)
          points
        else {
          val newPosition = position + velocity
          loop(newPosition, velocity - 1, points :+ newPosition)
        }
      }

      loop(0, velocity, Vector(0))
    }

    val tallestPossibleShot = (for {
      i <- 0 to Math.abs(input.bottomBound)
    } yield {
      simulateY(i)
    })
      .filter(_.exists(p => inRange(p, input.bottomBound, input.topBound)))
      .maxBy(_.length)

    // I have no proof that this will work every time, and I haven't thought too hard about where it doesn't
    tallestPossibleShot.max
  }

  def partTwo(input: Input): Int = {
    def inRange(num: Int, lower: Int, upper: Int): Boolean =
      num >= lower && num <= upper

    def simulate(velocity: Int, condition: (Int, Int) => Boolean): Vector[Int] = {
      def loop(position: Int, velocity: Int, points: Vector[Int]): Vector[Int] = {
        if (condition(position, velocity))
          points
        else {
          val newPosition = position + velocity
          loop(newPosition, velocity - 1, points :+ newPosition)
        }
      }

      loop(0, velocity, Vector(0))
    }

    def simulateX(velocity: Int): Vector[Int] = {
      simulate(velocity, (position, velocity) => velocity == 0 || (position + velocity > input.rightBound))
    }

    def simulateY(velocity: Int): Vector[Int] = {
      simulate(velocity, (position, velocity) => position + velocity < input.bottomBound)
    }

    val xSimulations =
      (0 to input.rightBound)
        .map(velocity => velocity -> simulateX(velocity))
        .filter(_._2.exists(inRange(_, input.leftBound, input.rightBound)))

    val ySimulations =
      (-Math.abs(input.bottomBound) to Math.abs(input.bottomBound))
        .map(velocity => velocity -> simulateY(velocity))
        .filter(_._2.exists(inRange(_, input.bottomBound, input.topBound)))

    val xRanges = xSimulations.map { case (velocity, positions) =>
      val rawRange = positions.zipWithIndex.filter { case (pos, _) => inRange(pos, input.leftBound, input.rightBound) }

      velocity -> Range(rawRange)
    }

    val yRanges = ySimulations.map { case (velocity, positions) =>
      val rawRange = positions.zipWithIndex.filter { case (pos, _) => inRange(pos, input.bottomBound, input.topBound)}

      velocity -> Range(rawRange)
    }

    val res = for {
      xRange <- xRanges
      yRange <- yRanges
      if xRange._2.intersects(yRange._2)
    } yield {
      (xRange._1, yRange._1)
    }

    res.length
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}