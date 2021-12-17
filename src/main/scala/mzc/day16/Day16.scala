package mzc.day16

import mzc.day16.PacketParser._

import scala.annotation.tailrec
import scala.io.Source

trait TreeNode {
  val version: Int
  def eval: Long
}
case class OperatorNode(version: Int, op: Operator, children: Vector[TreeNode]) extends TreeNode {
  def eval: Long = {
    val evalChildren = children.map(_.eval)
    op match {
      case Sum => evalChildren.sum
      case Product => evalChildren.product
      case Minimum => evalChildren.min
      case Maximum => evalChildren.max
      case GreaterThan => if (evalChildren(0) > evalChildren(1)) 1 else 0
      case LessThan => if (evalChildren(0) < evalChildren(1)) 1 else 0
      case EqualTo => if (evalChildren(0) == evalChildren(1)) 1 else 0
    }
  }
}

case class ValueNode(version: Int, num: Long) extends TreeNode {
  def eval: Long = num
}

object TreeNode {
  def sumVersion(root: TreeNode): Int = root match {
    case OperatorNode(version, _, children) => version + children.map(sumVersion).sum
    case ValueNode(version, _) => version
  }
}

trait Operator
case object Sum extends Operator
case object Product extends Operator
case object Minimum extends Operator
case object Maximum extends Operator
case object GreaterThan extends Operator
case object LessThan extends Operator
case object EqualTo extends Operator

object Operator {
  def apply(n: Int): Operator = n match {
    case 0 => Sum
    case 1 => Product
    case 2 => Minimum
    case 3 => Maximum
    case 5 => GreaterThan
    case 6 => LessThan
    case 7 => EqualTo
  }
}

object PacketParser {
  def parsePacket(dataStream: String): (TreeNode, String) = {
    val (version, packetType, dataTail) = parseHeader(dataStream)

    if (packetType == 4) {
      parseLiteralPacket(version, dataTail)
    } else {
      parseOperatorPacket(version, packetType, dataTail)
    }
  }

  def parseHeader(packet: String): (Int, Int, String) = {
    val (header, data) = read(packet, 6)
    val version = toIntBin(header.take(3))
    val packetType = toIntBin(header.drop(3))

    (version, packetType, data)
  }

  def parseLiteralPacket(version: Int, packet: String): (ValueNode, String) = {
    @tailrec
    def decodeVarbyte(packet: String, rawNumber: String): (ValueNode, String) = {
      val (pentet, data) = read(packet, 5)
      if (pentet.head == '1') {
        decodeVarbyte(data, rawNumber + pentet.tail)
      } else {
        (ValueNode(version, toLongBin(rawNumber + pentet.tail)), data)
      }
    }

    decodeVarbyte(packet, "")
  }

  def parseOperatorPacket(version: Int, packetType: Int, dataStream: String): (OperatorNode, String) = {
    val (lengthTypeId, dataTail) = readInt(dataStream, 1)
    if (lengthTypeId == 0) {
      parseLengthPackets(version, packetType, dataTail)
    } else {
      parseCountPackets(version, packetType, dataTail)
    }
  }

  def parseLengthPackets(version: Int, packetType: Int, packet: String): (OperatorNode, String) = {
    val (length, data) = readInt(packet, 15)
    val (subPacket, dataTail) = read(data, length)

    @tailrec
    def loop(dataStream: String, children: Vector[TreeNode]): Vector[TreeNode] = {
      if (dataStream.isEmpty) {
        children
      } else {
        val (childNode, newDataStream) = parsePacket(dataStream)
        loop(newDataStream, children :+ childNode)
      }
    }

    val children = loop(subPacket, Vector())
    val node = OperatorNode(version, Operator(packetType), children)

    (node, dataTail)
  }

  def parseCountPackets(version: Int, packetType: Int, packet: String): (OperatorNode, String) = {
    val (packetCount, dataTail) = readInt(packet, 11)

    @tailrec
    def loop(dataStream: String, children: Vector[TreeNode], counter: Int): (Vector[TreeNode], String) = {
      if (counter == packetCount)
        (children, dataStream)
      else {
        val (childNode, newDataStream) = parsePacket(dataStream)
        loop(newDataStream, children :+ childNode, counter + 1)
      }
    }

    val (children, dataResult) = loop(dataTail, Vector(), 0)
    val node = OperatorNode(version, Operator(packetType), children)
    (node, dataResult)
  }

  def read(stream: String, amount: Int): (String, String) = {
    (stream.take(amount), stream.drop(amount))
  }

  def readInt(stream: String, amount: Int): (Int, String) = {
    (toIntBin(stream.take(amount)), stream.drop(amount))
  }

  def toIntBin(s: String): Int = Integer.parseInt(s, 2)

  def toLongBin(s: String): Long = java.lang.Long.parseLong(s, 2)
}

object Day16 extends App {
  def hexToBinary(dataStream: String): String = {
    // TODO: wtf is this
    dataStream.flatMap(c => "%4s".format(Integer.parseInt(c.toString, 16).toBinaryString).replace(' ', '0'))
  }

  type Input = String

  def parseInput(): Input = {
    Source
      .fromResource("day16/input.txt")
      .getLines
      .toList
      .head
  }

  def partOne(input: Input): Int = {
    val hex = hexToBinary(input)

    val root = parsePacket(hex)._1
    TreeNode.sumVersion(root)
  }

  def partTwo(input: Input): Long = {
    val hex = hexToBinary(input)

    val root = parsePacket(hex)._1
    root.eval
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}