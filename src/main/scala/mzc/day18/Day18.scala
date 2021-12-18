package mzc.day18

import scala.annotation.tailrec
import scala.io.Source

case class Node(num: Int, depth: Int) {
  def mapNum(f: Int => Int): Node = {
    Node(f(num), depth)
  }

  def mapDepth(f: Int => Int): Node = {
    Node(num, f(depth))
  }
}

object Day18 extends App {
  def parse(line: String): Vector[Node] = {
    def loop(line: String, depth: Int): (Vector[Node], String) = {
      line.head match {
        case '[' =>
          val (left, dataStream) = loop(line.tail, depth + 1)
          val (right, dataStream1) = loop(dataStream.tail, depth + 1)
          (left ++ right, dataStream1.tail)
        case n => (Vector(Node(n.asDigit, depth)), line.tail)
      }
    }

    val res = loop(line, -1)
    res._1
  }

  def add(left: Vector[Node], right: Vector[Node]): Vector[Node] = {
    left.map(_.mapDepth(_ + 1)) ++ right.map(_.mapDepth(_ + 1))
  }

  @tailrec
  def reduce(nodes: Vector[Node]): Vector[Node] = {
    def constrain(num: Int, lower: Int, upper: Int): Int =
      if (num < lower)
        lower
      else if (num > upper)
        upper
      else
        num

    val maybeExplodeIndex = nodes.indexWhere(_.depth == 4)
    val maybeSplitIndex = nodes.indexWhere(_.num >= 10)

    if (maybeExplodeIndex >= 0) {
      val leftValue = nodes(maybeExplodeIndex).num
      val rightValue = nodes(maybeExplodeIndex + 1).num

      val maybeLeftNode = nodes.lift(maybeExplodeIndex - 1).map(_.mapNum(_ + leftValue)).map(Vector.apply(_)).getOrElse(Vector.empty)
      val maybeRightNode = nodes.lift(maybeExplodeIndex + 2).map(_.mapNum(_ + rightValue)).map(Vector.apply(_)).getOrElse(Vector.empty)
      val newNode = Node(0, nodes(maybeExplodeIndex).depth - 1)

      val newNodes =
        nodes.slice(0, constrain(maybeExplodeIndex - 1, 0, nodes.length)) ++
          maybeLeftNode ++
          Vector(newNode) ++
          maybeRightNode ++
          nodes.slice(constrain(maybeExplodeIndex + 3, 0, nodes.length), nodes.length)

      reduce(newNodes)
    } else if (maybeSplitIndex >= 0) {
      val originalNode = nodes(maybeSplitIndex)
      val newDepth = nodes(maybeSplitIndex).depth + 1
      val splitDepths = List(Node(originalNode.num / 2, newDepth), Node(Math.ceil(originalNode.num / 2.0).toInt, newDepth))

      val newNodes = nodes.slice(0, maybeSplitIndex) ++ splitDepths ++ nodes.slice(maybeSplitIndex + 1, nodes.length)
      reduce(newNodes)
    } else {
      nodes
    }
  }

  def magnitude(nodes: Vector[Node]): Int = {
    @tailrec
    def loop(nodes: Vector[Node]): Node = {
      if (nodes.length == 1)
        return nodes.head
      val firstPairIndex = nodes.sliding(2).indexWhere(nodePair => nodePair(0).depth == nodePair(1).depth)
      val pairMagnitude = nodes(firstPairIndex).num * 3 + nodes(firstPairIndex + 1).num * 2
      val pairDepth = nodes(firstPairIndex).depth - 1
      val newPair = Vector(Node(pairMagnitude, pairDepth))

      loop(nodes.slice(0, firstPairIndex) ++ newPair ++ nodes.slice(firstPairIndex + 2, nodes.length))
    }

    loop(nodes).num
  }

  type Input = List[String]

  def parseInput(): Input = {
    Source
      .fromResource("day18/input.txt")
      .getLines
      .toList
  }

  def partOne(input: Input): Int = {
    val snailfishNumbers = input.map(parse)

    val sum = snailfishNumbers.reduceLeft { (left, right) =>
      reduce(add(left, right))
    }

    magnitude(sum)
  }

  def partTwo(input: Input): Int = {
    val snailfishNumbers = input.map(parse)

    val allSums = for {
      first <- snailfishNumbers
      second <- snailfishNumbers
    } yield magnitude(reduce(add(first, second)))

    allSums.max
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}