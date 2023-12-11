package com.github.desavitsky.day10

import com.github.desavitsky.day10.Direction.*
import scala.annotation.tailrec
import scala.io.Source

enum Direction:
  case North, East, West, South

object Direction:
  def opposite(direction: Direction): Direction = direction match
    case Direction.North => Direction.South
    case Direction.East  => Direction.West
    case Direction.West  => Direction.East
    case Direction.South => Direction.North

enum Pipe(val repr: String, val connects: Set[Direction]):
  case NS extends Pipe("|", Set(North, South))
  case EW extends Pipe("-", Set(East, West))
  case NE extends Pipe("L", Set(North, East))
  case NW extends Pipe("J", Set(North, West))
  case SW extends Pipe("7", Set(South, West))
  case SE extends Pipe("F", Set(South, East))

  case Start  extends Pipe("S", Direction.values.toSet)
  case Ground extends Pipe(".", Set.empty)

  def connectsTo(direction: Direction): Boolean =
    this.connects.contains(Direction.opposite(direction))

  override def toString: String = repr

object Pipe:
  def fromString(str: String): Option[Pipe] =
    Pipe.values.find(_.repr == str)

case class Position(x: Int, y: Int):
  def nextPosition(direction: Direction): Position = direction match
    case Direction.North => this.copy(x = x - 1)
    case Direction.East  => this.copy(y = y + 1)
    case Direction.West  => this.copy(y = y - 1)
    case Direction.South => this.copy(x = x + 1)

  def vicinity: Set[Position] =
    Set(
      this.copy(x = x + 1),
      this.copy(x = x - 1),
      this.copy(y = y + 1),
      this.copy(y = y - 1),
      this.copy(x = x + 1, y = y - 1),
      this.copy(x = x + 1, y = y + 1),
      this.copy(x = x - 1, y = y + 1),
      this.copy(x = x - 1, y = y - 1),
    )

@tailrec
def cycle(position: Position, field: Vector[Vector[Pipe]], direction: Direction)(acc: Set[Position]): Set[Position] =
  val nextPosition = position.nextPosition(direction)
  val nextPipe     = field(nextPosition.x)(nextPosition.y)
  if nextPipe == Pipe.Start then acc + nextPosition
  else
    val nextDirection = nextPipe.connects.filter(_ != Direction.opposite(direction)).head
    cycle(nextPosition, field, nextDirection)(acc + nextPosition)

def fill(direction: Direction, pipe: Pipe, position: Position): Set[Position] =
  (direction, pipe) match
    case (North, Pipe.NS) => Set(position.copy(y = position.y + 1))
    case (South, Pipe.NS) => Set(position.copy(y = position.y - 1))
    case (East, Pipe.EW)  => Set(position.copy(x = position.x + 1))
    case (West, Pipe.EW)  => Set(position.copy(x = position.x - 1))
    case (South, Pipe.NE) =>
      Set(position.copy(x = position.x + 1), position.copy(y = position.y - 1), Position(position.x + 1, position.y - 1))
    case (West, Pipe.NE)  =>
      Set(position.copy(x = position.x - 1), position.copy(y = position.y + 1), Position(position.x - 1, position.y + 1))
    case (South, Pipe.NW) =>
      Set(position.copy(x = position.x - 1), position.copy(y = position.y - 1), Position(position.x - 1, position.y - 1))
    case (East, Pipe.NW)  =>
      Set(position.copy(x = position.x + 1), position.copy(y = position.y + 1), Position(position.x + 1, position.y + 1))
    case (North, Pipe.SW) =>
      Set(position.copy(x = position.x - 1), position.copy(y = position.y + 1), Position(position.x - 1, position.y + 1))
    case (East, Pipe.SW)  =>
      Set(position.copy(x = position.x + 1), position.copy(y = position.y - 1), Position(position.x + 1, position.y - 1))
    case (North, Pipe.SE) =>
      Set(position.copy(x = position.x + 1), position.copy(y = position.y + 1), Position(position.x + 1, position.y + 1))
    case (West, Pipe.SE)  =>
      Set(position.copy(x = position.x - 1), position.copy(y = position.y - 1), Position(position.x - 1, position.y - 1))
    case _                => Set.empty

def cycleAndFill(field: Vector[Vector[Pipe]], groundPoints: Set[Position], position: Position, direction: Direction)(
  filled: Set[Position]
): Set[Position] =
  val nextPosition = position.nextPosition(direction)
  val nextPipe     = field(nextPosition.x)(nextPosition.y)
  if nextPipe == Pipe.Start then {

    @tailrec
    def fillRemaining(groundPoints: Set[Position], filled: Set[Position]): Set[Position] =
      val newFilled = groundPoints.diff(filled).filter(p => p.vicinity.intersect(filled).nonEmpty)
      println(s"NF: $newFilled")
      if newFilled.nonEmpty then fillRemaining(groundPoints, filled ++ newFilled)
      else filled

    println(s"GP: $groundPoints")
    fillRemaining(groundPoints, filled)
  } else
    val nextDirection = nextPipe.connects.filter(_ != Direction.opposite(direction)).head
    val filledUpdated =
      fill(direction, field(nextPosition.x)(nextPosition.y), nextPosition).intersect(groundPoints) ++ filled
    println(
      s"POSITION: $nextPosition, PIPE: ${field(nextPosition.x)(nextPosition.y)} DIRECTION: ${direction} FIILED: ${fill(direction, field(nextPosition.x)(nextPosition.y), nextPosition)
          .intersect(groundPoints)}"
    )
    cycleAndFill(field, groundPoints, nextPosition, nextDirection)(filledUpdated)

@main def run =
  val field = Source
    .fromResource("day10/task.txt")
    .getLines()
    .toVector
    .map(_.flatMap(s => Pipe.fromString(s.toString)).toVector)

  val startPosition = field.zipWithIndex
    .collectFirst {
      case (pipes, index) if pipes.contains(Pipe.Start) =>
        (index, pipes.indexWhere(_ == Pipe.Start))
    }
    .map(Position(_, _))
    .getOrElse(Position(0, 0))

  val direction = Direction.values
    .collect {
      case direction if {
            val next = startPosition.nextPosition(direction)
            field.lift(next.x).flatMap(_.lift(next.y)).exists(_.connectsTo(direction))
          } =>
        direction
    }
    .lastOption
    .getOrElse(throw new IllegalArgumentException("No eligible pipes around start"))

  val loop = cycle(startPosition, field, direction)(Set.empty)

  val allOutOfTheLoop = field.zipWithIndex
    .map { case (line, index) =>
      line.zipWithIndex
        .collect { case (pipe, rowIndex) if !loop.contains(Position(index, rowIndex)) => rowIndex }
        .map(Position(index, _))

    }

  val res1 = loop.size / 2

  val res2 = cycleAndFill(field, allOutOfTheLoop.flatten.toSet, startPosition, direction)(Set.empty).size

  println(res1)
  println(res2)
