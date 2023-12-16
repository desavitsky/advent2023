package com.github.desavitsky.day2

import scala.annotation.tailrec
import scala.io.Source

object Part2:

  case class MinimalGame(
    id: Int,
    power: Int
  )

  object Sets:

    def unapply(str: String): Option[Vector[Color]] =
      Some(
        str
          .split(";")
          .toVector
          .flatMap(Set.unapply)
          .flatten
      )

  object Set:
    def unapply(set: String): Option[Set[Color]] =
      val splittedSeq = set.split(",").map(_.strip)
      val colorsCount = splittedSeq.length
      val colors      = set
        .split(",")
        .map(_.strip)
        .flatMap(Color.unapply)
      if colors.length == colorsCount then Some(colors.toSet)
      else None

  object MinimalGame:
    private val gameRegex = "Game ([\\d]+):(.*)".r

    @tailrec
    def collectFirstColors(colors: List[Color])(acc: Vector[Color]): Vector[Color] =
      colors match {
        case head :: tail if acc.size < 3 =>
          head match
            case Blue(count) if acc.collectFirst { case Blue(c) => c }.isDefined => collectFirstColors(tail)(acc)

            case Green(count) if acc.collectFirst { case Green(c) => c }.isDefined => collectFirstColors(tail)(acc)
            case Red(count) if acc.collectFirst { case Red(c) => c }.isDefined     => collectFirstColors(tail)(acc)
            case notYet                                                            => collectFirstColors(tail)(acc :+ notYet)
        case _                            => acc
      }

    def unapply(game: String): Option[MinimalGame] = game match
      case gameRegex(AsInt(id), Sets(sets)) =>
        val sorted    = sets.sortBy(-_.count).toList
        val colorsMin = collectFirstColors(sorted.tail)(Vector(sorted.head))
        Some(MinimalGame(id, colorsMin.map(_.count).product))
      case _                                => None

  object AsInt:
    def unapply(str: String): Option[Int] =
      str.toIntOption

  sealed trait Color(val count: Int)

  object Color:
    def unapply(str: String): Option[Color] = str match
      case s"${AsInt(count)} blue"  => Some(Blue(count))
      case s"${AsInt(count)} red"   => Some(Red(count))
      case s"${AsInt(count)} green" => Some(Green(count))
      case _                        => None

  case class Red(override val count: Int)   extends Color(count)
  case class Green(override val count: Int) extends Color(count)
  case class Blue(override val count: Int)  extends Color(count)

@main def run2 =
  val result = Source
    .fromResource("day2/input.txt")
    .getLines()
    .toVector
    .flatMap(Part2.MinimalGame.unapply)
    .map(_.power)
    .sum
  println(result)
