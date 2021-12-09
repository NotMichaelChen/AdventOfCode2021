package mzc.day8

import scala.io.Source

object Day8 extends App {
  case class Entry(
                    signals: List[String],
                    output: List[String]
                  )

  case class Mapping(
                      signal: Char,
                      potentialSegments: Set[Char]
                    )

  def decodeEntry(entry: Entry): List[Int] = {
    val allSegments = Set('a', 'b', 'c', 'd', 'e', 'f', 'g')

    // Map over each individual signal per signals cluster and assign candidate segments based on the number of signals
    val initialSegmentMappings = entry.signals.flatMap { signal =>
      signal.length match {
        case 2 => signal.map(Mapping(_, Set('c', 'f')))
        case 3 => signal.map(Mapping(_, Set('a', 'c', 'f')))
        case 4 => signal.map(Mapping(_, Set('b', 'c', 'd', 'f')))
        case 5 => allSegments.diff(signal.toSet).toList.map(Mapping(_, Set('b', 'c', 'e', 'f')))
        case 6 => List(Mapping(allSegments.diff(signal.toSet).head, Set('d', 'c', 'e')))
        case 7 => signal.map(Mapping(_, allSegments))
      }
    }

    // Group by signal and intersect all candidate segment sets for that signal
    val finalMappings = initialSegmentMappings.groupMap(_.signal)(_.potentialSegments).map { case (signal, segments) =>
      Mapping(signal, segments.foldLeft(allSegments)(_ intersect _))
    }

    // Pick a direct mapping if it exists
    // If it does, add it to the true mappings, remove it from the candidate mappings, then filter out that segment from
    // the rest of the candidate mappings
    def loop(candidateMappings: List[Mapping], trueMappings: Map[Char, Char]): Map[Char, Char] = {
      val trueMappingOpt = candidateMappings.find(_.potentialSegments.size == 1)
      trueMappingOpt match {
        case Some(trueMapping) =>
          val filteredMappings = candidateMappings.filterNot(_.signal == trueMapping.signal).map(mapping => mapping.copy(potentialSegments = mapping.potentialSegments.filterNot(_ == trueMapping.potentialSegments.head)))

          val b = trueMappings + (trueMapping.signal -> trueMapping.potentialSegments.head)

          loop(filteredMappings, b)

        case None => trueMappings
      }
    }

    val trueMapping = loop(finalMappings.toList, Map.empty)

    if (trueMapping.keySet != allSegments) {
      throw new Exception(s"Error, unable to deduce $entry")
    }

    val segmentMap = Map(
      Set('a', 'b', 'c', 'e', 'f', 'g') -> 0,
      Set('c', 'f') -> 1,
      Set('a', 'c', 'd', 'e', 'g') -> 2,
      Set('a', 'c', 'd', 'f', 'g') -> 3,
      Set('b', 'c', 'd', 'f') -> 4,
      Set('a', 'b', 'd', 'f', 'g') -> 5,
      Set('a', 'b', 'd', 'e', 'f', 'g') -> 6,
      Set('a', 'c', 'f') -> 7,
      Set('a', 'b', 'c', 'd', 'e', 'f', 'g') -> 8,
      Set('a', 'b', 'c', 'd', 'f', 'g') -> 9
    )

    entry.output.map { segmentsString =>
      val activeSegments = segmentsString.map(trueMapping.apply).toSet

      segmentMap(activeSegments)
    }
  }

  type Input = List[Entry]

  def parseInput(): Input = {
    Source
      .fromResource("day8/input.txt")
      .getLines
      .toList
      .map {
        case s"$signals|$output" =>
          Entry(
            signals.strip.split("\\s+").toList,
            output.strip.split("\\s+").toList
          )
      }
  }

  def partOne(input: Input): Int = {
    val easyNumbers = Set(1, 4, 7, 8)
    input.flatMap { entry =>
      decodeEntry(entry)
    }
      .count(easyNumbers.contains)
  }

  def partTwo(input: Input): Int = {
    input.map { entry =>
      decodeEntry(entry).mkString.toInt
    }
      .sum
  }

  val input = parseInput()

  println(partOne(input))
  println(partTwo(input))
}