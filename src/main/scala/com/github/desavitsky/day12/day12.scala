package com.github.desavitsky.day12

import scala.io.Source

def parseLine(line: String, groups: Vector[Int]): Long =

  val size = line.length

  def parseSubString(subString: String)(currentGroup: Int, prevGroups: Vector[Int])(
    memo: Map[Int, Vector[Vector[Int]]]
  ): Vector[Vector[Int]] =
    if memo.contains(subString.length) && currentGroup == 0 then
      memo
        .getOrElse(subString.length, Vector.empty)
        .map(_.prependedAll(prevGroups))
        .filter(groups.endsWith(_))
    else if subString.isEmpty then Vector(prevGroups ++ Vector(currentGroup).filter(_ != 0)).filter(groups.endsWith(_))
    else
      subString.head match
        case '.' => parseSubString(subString.tail)(0, prevGroups ++ Vector(currentGroup).filter(_ != 0))(memo)
        case '#' => parseSubString(subString.tail)(currentGroup + 1, prevGroups)(memo)
        case '?' =>
          parseSubString(subString.tail)(0, prevGroups ++ Vector(currentGroup).filter(_ != 0))(memo) ++
            parseSubString(subString.tail)(currentGroup + 1, prevGroups)(memo)

  def parseSubString2(subString: String)(currentGroup: Int, prevGroups: Vector[Int])(
    memo: Map[Int, Map[Vector[Int], Long]]
  ): Map[Vector[Int], Long] =
    if memo.contains(subString.length) && currentGroup == 0 then
      val res = memo
        .getOrElse(subString.length, Map.empty)
        .map((key, q) => key.prependedAll(prevGroups) -> q)
        .view
        .filterKeys(groups.endsWith(_))
        .toMap
      res
    else if subString.isEmpty then
      val upd = prevGroups ++ Vector(currentGroup).filter(_ != 0)
      val res =
        if groups.endsWith(upd) then Map(upd -> 1L)
        else Map.empty
      res
    else
      subString.head match
        case '.' => parseSubString2(subString.tail)(0, prevGroups ++ Vector(currentGroup).filter(_ != 0))(memo)
        case '#' => parseSubString2(subString.tail)(currentGroup + 1, prevGroups)(memo)
        case '?' =>
          val left  = parseSubString2(subString.tail)(0, prevGroups ++ Vector(currentGroup).filter(_ != 0))(memo)
          val right = parseSubString2(subString.tail)(currentGroup + 1, prevGroups)(memo)
          val res   = right ++ left.map((combs, q) => combs -> (right.getOrElse(combs, 0L) + q))
          res

  val res = (1 to size)
    .foldLeft(Map.empty[Int, Map[Vector[Int], Long]]) { case (memo, i) =>
      val res = parseSubString2(line.takeRight(i))(0, Vector.empty)(memo)
      memo.updated(i, res)
    }

  res.getOrElse(size, Map.empty).filter(_._1 == groups).values.sum

@main def run2 =
  val input = Source
    .fromResource("day12/task.txt")
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
