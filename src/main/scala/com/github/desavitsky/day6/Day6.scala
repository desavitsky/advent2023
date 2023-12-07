package com.github.desavitsky.day6

import scala.annotation.nowarn
import scala.io.Source

case class Race(
  time: Long,
  distance: Long
):

  private def ceil(d: Double) =
    val ceiled = Math.ceil(d)
    if d == ceiled then (d + 1).toLong
    else ceiled.toLong

  private def floor(d: Double) =
    val floored = Math.floor(d)
    if d == floored then (d - 1).toLong
    else floored.toLong

  def waysToBeat: Long =
    val d   = Math.sqrt(time * time - 4 * distance)
    val min = ceil((time - d) / 2)
    val max = floor((time + d) / 2)
    val res = max - min + 1
    res

@nowarn @main def run =
  val input = Source
    .fromResource("day6/task.txt")
    .getLines()
    .toVector
    .map { str =>
      str
        .stripPrefix("Time:")
        .stripPrefix("Distance:")
        .strip()
        .split("\\s+")
        .map(_.toLong)
        .toVector
    } match
    case Vector(time, distance) => time.zip(distance).map(Race(_, _))

  val input2 = Source
    .fromResource("day6/task.txt")
    .getLines()
    .toVector
    .map { str =>
      println(
        str
          .stripPrefix("Time:")
          .stripPrefix("Distance:")
          .strip()
          .filter(_.isDigit)
          .toLong
      )
      str
        .stripPrefix("Time:")
        .stripPrefix("Distance:")
        .strip()
        .filter(_.isDigit)
        .toLong
    } match
    case Vector(time, distance) => Race(time, distance)

  val res1 = input.map(_.waysToBeat).product
  println(res1)

  val res2 = input2.waysToBeat
  println(input2)
  println(res2)
