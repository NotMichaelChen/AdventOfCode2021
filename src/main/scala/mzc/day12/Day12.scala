package mzc.day12

import scala.io.Source

object Day12 extends App {
  def exploreGraph(input: Input, filterNextNodes: (String, Set[String], Vector[String]) => Set[String]): Int = {
    val reversePaths = input.map(_.swap)

    val graph = (input ++ reversePaths).foldLeft(Map.empty: Map[String, Set[String]]) { case (acc, next) =>
      acc.get(next._1) match {
        case Some(nextSet) => acc + (next._1 -> (nextSet + next._2))
        case None => acc + (next._1 -> Set(next._2))
      }
    }

    def loop(graph: Map[String, Set[String]], currentPath: Vector[String], currentNode: String): Int = {
      if (currentNode == "end") {
        return 1
      }

      val nextNodes = graph.getOrElse(currentNode, Set.empty).filter(next => next != "start")
      val filteredNextNodes = filterNextNodes(currentNode, nextNodes, currentPath)

      if (filteredNextNodes.isEmpty) {
        0
      } else {
        filteredNextNodes.toList.map(next => loop(graph, currentPath :+ currentNode, next)).sum
      }
    }

    loop(graph, Vector.empty, "start")
  }

  type Input = List[(String, String)]

  def parseInput(): Input = {
    Source
      .fromResource("day12/input.txt")
      .getLines
      .toList
      .map {
        case s"$from-$to" => from -> to
      }
  }

  def partOne(input: Input): Int = {
    def isUpper(str: String) = str.map(_.isUpper).forall(identity)

    def filterNextNodes(currentNode: String, candidates: Set[String], currentPath: Vector[String]): Set[String] = {
      candidates.filter(next => isUpper(next) || !currentPath.contains(next))
    }

    exploreGraph(input, filterNextNodes)
  }

  def partTwo(input: Input): Int = {
    def isUpper(str: String) = str.map(_.isUpper).forall(identity)

    // Assumes that "start" is not in the candidate set
    def filterNextNodes(currentNode: String, candidates: Set[String], currentPath: Vector[String]): Set[String] = {
      def findSmallDuplicate(path: Vector[String]): Option[String] = {
        path.filter(p => !isUpper(p)).groupBy(identity).collectFirst { case (elem, duplicates) if duplicates.length > 1 => elem }
      }

      candidates.filter { next =>
        isUpper(next) ||
          (findSmallDuplicate(currentPath :+ currentNode).isEmpty) ||
          (!currentPath.contains(next))
      }
    }

    exploreGraph(input, filterNextNodes)
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}