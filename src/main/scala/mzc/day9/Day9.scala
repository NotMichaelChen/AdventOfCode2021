package mzc.day9

import scala.annotation.tailrec
import scala.io.Source

case class Point(row: Int, column: Int) {
  def tuple: (Int, Int) = (row, column)
}

object Point {
  def apply(tuple: (Int, Int)): Point = Point(tuple._1, tuple._2)
}

case class HeightMap(map: Vector[Vector[Int]]) {
  val height: Int = map.length
  val length: Int = map.head.length

  def get(point: Point): Int = map(point.row)(point.column)

  def getOpt(point: Point): Option[Int] = {
    for {
      rowValue <- map.lift(point.row)
      columnValue <- rowValue.lift(point.column)
    } yield columnValue
  }

  def adjacentIndices(point: Point): Vector[Point] = {
    val (row, column) = point.tuple

    val rows = Vector(row, row, row - 1, row + 1)
    val columns = Vector(column - 1, column + 1, column, column)

    rows.zip(columns).map(Point.apply).filter(p => getOpt(p).isDefined)
  }

  def adjacent(point: Point): Vector[Int] = {
    adjacentIndices(point).map(get)
  }

  def riskLevel(point: Point): Int = {
    val currentHeight = get(point)
    val adjacentHeights = adjacent(point)

    if (adjacentHeights.forall(currentHeight < _))
      currentHeight + 1
    else
      0
  }

  def fill(currentLocation: Point): Set[Point] = {
    @tailrec
    def loop(currentLocation: Point, candidates: Set[Point], visited: Set[Point]): Set[Point] = {
      val updatedVisited = visited + currentLocation

      val newCandidates = adjacentIndices(currentLocation).filterNot(p => get(p) == 9 || visited.contains(p))

      val updatedCandidates = candidates.union(newCandidates.toSet)

      if (updatedCandidates.isEmpty) {
        updatedVisited
      } else {
        loop(updatedCandidates.head, updatedCandidates.tail, updatedVisited)
      }
    }

    loop(currentLocation, Set.empty, Set.empty)
  }
}

object Day9 extends App {
  type Input = Vector[Vector[Int]]

  def parseInput(): Input = {
    Source
      .fromResource("day9/input.txt")
      .getLines
      .toVector
      .map(_.map(_.asDigit).toVector)
  }

  def partOne(input: Input): Int = {
    val heightMap = HeightMap(input)

    val lowestRiskLevels =
      for {
        row <- 0 until heightMap.height
        column <- 0 until heightMap.length
      } yield {
        heightMap.riskLevel(Point(row, column))
      }

    lowestRiskLevels.sum
  }

  def partTwo(input: Input): Int = {
    val heightMap = HeightMap(input)

    val startingPoints = for {
      row <- 0 until heightMap.height
      column <- 0 until heightMap.length

      point = Point(row, column)

      if heightMap.get(point) != 9
    } yield point

    @tailrec
    def loop(startingPositions: List[Point], basins: List[Set[Point]]): List[Set[Point]] = {
      if (startingPositions.isEmpty) {
        basins
      } else {
        val startingPos = startingPositions.head
        val basin = heightMap.fill(startingPos)

        val filteredStartingPositions = startingPositions.filterNot(basin.contains)

        loop(filteredStartingPositions, basins :+ basin)
      }
    }

    val basins = loop(startingPoints.toList, List.empty)
    basins.map(_.size).sorted(Ordering.Int.reverse).take(3).product
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}