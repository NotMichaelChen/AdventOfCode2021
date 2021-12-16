package mzc.day15

import mzc.util.{Matrix, Point}

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Ordering.Implicits._

trait RiskMatrix {
  def get(point: Point): Int

  def cardinalAdjacentIndices(point: Point): Vector[Point]
}

case class StandardMatrix(matrix: Matrix[Int]) extends RiskMatrix {
  override def get(point: Point): Int = matrix.get(point)

  override def cardinalAdjacentIndices(point: Point): Vector[Point] = matrix.cardinalAdjacentIndices(point)
}

case class WrappingMatrix(matrix: Matrix[Int]) extends RiskMatrix {
  private val rawHeight = matrix.matrix.length
  private val rawWidth = matrix.matrix.head.length
  private val height = rawHeight * 5
  private val width = rawWidth * 5

  def get(point: Point): Int = {
    def wrap(num: Int): Int = if (num > 9) num - 9 else num

    val rawPoint = Point(point.row % rawHeight, point.column % rawWidth)
    val rawValue = matrix.get(rawPoint)

    val additive = point.row / rawHeight + point.column / rawWidth

    wrap(rawValue + additive)
  }

  def cardinalAdjacentIndices(point: Point): Vector[Point] = {
    val (row, column) = point.tuple

    val rows = Vector(row, row, row - 1, row + 1)
    val columns = Vector(column - 1, column + 1, column, column)

    rows.zip(columns).map(Point.apply).filterNot { p =>
      p.row < 0 ||
        p.row >= height ||
        p.column < 0 ||
        p.column >= width
    }
  }
}

object Day15 extends App {
  def calculateCostMap(riskMap: RiskMatrix): Map[Point, Int] = {
    @tailrec
    def loop(riskMap: RiskMatrix, costMap: Map[Point, Int], inspectionQueue: Vector[Point]): Map[Point, Int] = {
      if (inspectionQueue.isEmpty)
        return costMap

      val currentPoint = inspectionQueue.head
      val currentCost = costMap(currentPoint) // Assume costMap is defined for the current point
      val adjacentPoints = riskMap.cardinalAdjacentIndices(currentPoint)

      val updatedPoints =
        adjacentPoints.flatMap { p =>
          val neighborCurrentCost = costMap.getOrElse(p, Int.MaxValue)
          val neighborPotentialCost = currentCost + riskMap.get(p)

          Option.when(neighborPotentialCost < neighborCurrentCost)(p -> neighborPotentialCost)
        }
          .toMap

      loop(riskMap, costMap ++ updatedPoints, inspectionQueue.tail ++ updatedPoints.keys)
    }

    loop(riskMap, Map(Point(0, 0) -> 0), Vector(Point(0, 0)))
  }

  def solution(riskMap: RiskMatrix): Int = {
    val result = calculateCostMap(riskMap)

    val largestPoint = result.keys.foldLeft(Point(0, 0)) { case (maxPoint, candidate) =>
      if (maxPoint.tuple > candidate.tuple)
        maxPoint
      else
        candidate
    }

    result(largestPoint)
  }

  type Input = Matrix[Int]

  def parseInput(): Input = {
    val rawMatrix =
      Source
        .fromResource("day15/input.txt")
        .getLines
        .toVector
        .map(_.map(_.asDigit).toVector)

    Matrix(rawMatrix)
  }

  def partOne(input: Input): Int = {
    solution(StandardMatrix(input))
  }

  def partTwo(input: Input): Int = {
    solution(WrappingMatrix(input))
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}