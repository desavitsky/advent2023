package com.github.desavitsky.day18

import com.github.desavitsky.day18.Direction.Down
import scala.annotation.tailrec
import scala.io.Source

enum Direction:
  case Right, Down, Left, Up

object Direction:
  def fromString(str: String): Option[Direction] =
    Direction.values.find(_.productPrefix.head.toString.toLowerCase == str.toLowerCase)

  def fromNumber(ord: Int): Option[Direction] = Direction.values.find(_.ordinal == ord)

case class Dig(
  direction: Direction,
  number: Int
)

case class Position(x: Int, y: Int):
  def dig(dig: Dig): Position =
    dig.direction match
      case Direction.Up    => copy(x = x - dig.number)
      case Direction.Down  => copy(x = x + dig.number)
      case Direction.Left  => copy(y = y - dig.number)
      case Direction.Right => copy(y = y + dig.number)

  def manhDistance(other: Position): Int = Math.abs(other.x - x) + Math.abs(other.y - y)

@tailrec
def findAllVertices(digPlan: List[Dig])(coords: List[Position]): List[Position] =
  digPlan.headOption match
    case None      => coords
    case Some(dig) => findAllVertices(digPlan.tail)(coords.head.dig(dig) :: coords)

case class Field(positions: List[Position]):
  lazy val innerArea: Long =
    @tailrec
    def findArea(points: List[Position])(acc: Long): Long =
      points match
        case head :: next if next.nonEmpty =>
          findArea(next)(acc + (next.head.x.toLong * head.y.toLong - head.x.toLong * next.head.y.toLong))
        case _                             => acc / 2
    Math.abs(findArea(positions)(0))

  lazy val perimeter: Int =
    positions
      .sliding(2)
      .map { pair =>
        pair.head.manhDistance(pair.last)
      }
      .sum
  lazy val totalArea      = innerArea + perimeter / 2 + 1

case class Border(left: Int, right: Option[Int] = None):
  val length: Int = right.map(_ - left + 1).getOrElse(1)

  val isHorizontal: Boolean = right.isDefined

object Border:
  given Ordering[Border] with
    override def compare(x: Border, y: Border): Int = x.left - y.left

@main def run =
  val rawInput = Source.fromResource("day18/input.txt").getLines().toVector
  val digPlan  = rawInput
    .map(_.split(" "))
    .flatMap { input =>
      input.headOption
        .flatMap(Direction.fromString)
        .map(Dig(_, input(1).toInt))
    }

  val digPlan2 = rawInput
    .map(_.split(" "))
    .flatMap { input =>
      input.headOption
        .flatMap(Direction.fromString)
        .flatMap(_ =>
          val preprocessed = input(2).stripPrefix("(").stripSuffix(")")
          val length       = java.lang.Long.parseLong(preprocessed.drop(1).dropRight(1), 16)
          val direction    = Direction.fromNumber(preprocessed.takeRight(1).toInt)
          direction.map(Dig(_, length.toInt))
        )
    }

  def findAndAdjustVertices(digPlan: Vector[Dig]): List[Position] =
    val fieldVertices = findAllVertices(digPlan.toList)(List(Position(0, 0)))
    val (xMin, xMax)  =
      val sortedX = fieldVertices.sortBy(_.x)
      sortedX.head.x -> sortedX.last.x

    val (yMin, yMax) =
      val sortedX = fieldVertices.sortBy(_.y)
      sortedX.head.y -> sortedX.last.y
    fieldVertices.map(pos => Position(pos.x - xMin, pos.y - yMin))

  val points1 = findAndAdjustVertices(digPlan)
  val points2 = findAndAdjustVertices(digPlan)

  val field1 = Field(points2)
  val field2 = Field(points2)
  val res1   = field1.totalArea
  val res2   = field2.totalArea
  println(res1)
  println(res2)
