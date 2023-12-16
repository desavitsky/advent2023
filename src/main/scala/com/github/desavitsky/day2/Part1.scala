package com.github.desavitsky.day2

import scala.annotation.tailrec
import scala.io.Source

object Part1:

  object MaxCount:
    val RedCubes   = 12
    val GreenCubes = 13
    val BlueCubes  = 14

  case class Game(
    id: Int,
    sets: List[Set[Color]]
  )

  object Sets:

    def unapply(str: String): Option[List[Set[Color]]] =
      val strSet = str.split(";").toList

      @tailrec
      def rev(strSet: List[String])(acc: List[Set[Color]]): Option[List[Set[Color]]] =
        strSet match
          case Set(colorSet) :: tail => rev(tail)(colorSet :: acc)
          case _ :: tail             => None
          case Nil                   => Some(acc)
      rev(strSet)(List.empty)

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

  object Game:
    private val gameRegex = "Game ([\\d]+):(.*)".r

    def unapply(game: String): Option[Game] = game match
      case gameRegex(AsInt(id), Sets(sets)) =>
        Some(Game(id, sets))
      case _                                => None

  object AsInt:
    def unapply(str: String): Option[Int] =
      str.toIntOption

  sealed trait Color

  object Color:
    def unapply(str: String): Option[Color] = str match
      case s"${AsInt(count)} blue" if count <= MaxCount.BlueCubes   => Some(Blue(count))
      case s"${AsInt(count)} red" if count <= MaxCount.RedCubes     => Some(Red(count))
      case s"${AsInt(count)} green" if count <= MaxCount.GreenCubes => Some(Green(count))
      case _                                                        => None

  case class Red(count: Int)   extends Color
  case class Green(count: Int) extends Color
  case class Blue(count: Int)  extends Color

@main def run =
  val result = Source
    .fromResource("day2/input.txt")
    .getLines()
    .toVector
    .flatMap(Part1.Game.unapply)
    .map(_.id)
    .sum
  println(result)
