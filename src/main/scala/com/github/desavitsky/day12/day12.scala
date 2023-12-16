package com.github.desavitsky.day12

import scala.io.Source

def parseLine(line: String, groups: Vector[Int]): Long =

  val size = line.length

  def parseSubString(subString: String)(currentGroup: Int, prevGroups: Vector[Int])(
    memo: Map[Int, Map[Vector[Int], Long]]
  ): Map[Vector[Int], Long] =
    if memo.contains(subString.length) && currentGroup == 0 then
      memo
        .getOrElse(subString.length, Map.empty)
        .map((key, q) => key.prependedAll(prevGroups) -> q)
        .view
        .filterKeys(groups.endsWith(_))
        .toMap
    else if subString.isEmpty then
      val upd = prevGroups ++ Vector(currentGroup).filter(_ != 0)
      if groups.endsWith(upd) then Map(upd -> 1L)
      else Map.empty
    else
      subString.head match
        case '.' => parseSubString(subString.tail)(0, prevGroups ++ Vector(currentGroup).filter(_ != 0))(memo)
        case '#' => parseSubString(subString.tail)(currentGroup + 1, prevGroups)(memo)
        case '?' =>
          val left  = parseSubString(subString.tail)(0, prevGroups ++ Vector(currentGroup).filter(_ != 0))(memo)
          val right = parseSubString(subString.tail)(currentGroup + 1, prevGroups)(memo)
          right ++ left.map((combs, q) => combs -> (right.getOrElse(combs, 0L) + q))

  (1 to size)
    .foldLeft(Map.empty[Int, Map[Vector[Int], Long]]) { case (memo, i) =>
      val res = parseSubString(line.takeRight(i))(0, Vector.empty)(memo)
      memo.updated(i, res)
    }
    .getOrElse(size, Map.empty)
    .filter(_._1 == groups)
    .values
    .sum

@main def run =
  val input = Source
    .fromResource("day12/input.txt")
    .getLines()
    .toVector
    .map { str =>
      val split = str.split(" ")
      split.head -> split.last.split(",").map(_.toInt).toVector
    }

  val res1 = input.map { case (line, groups) =>
    parseLine(line, groups)
  }.sum

  val res2 = input.map { case (line, groups) =>
    parseLine(Vector.fill(5)(line).mkString("?"), Vector.fill(5)(groups).flatten)
  }.sum

  println(res1)
  println(res2)
