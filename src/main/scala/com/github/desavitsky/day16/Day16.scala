package com.github.desavitsky.day16

import scala.annotation.tailrec
import scala.io.Source

enum Tile(val repr: Char):
  case Empty     extends Tile('.')
  case LRMirror  extends Tile('/')
  case RLMirror  extends Tile('\\')
  case VSplitter extends Tile('|')
  case HSplitter extends Tile('-')

object Tile:
  def fromString(str: Char): Option[Tile] =
    Tile.values.find(_.repr == str)

enum Direction:
  case Up, Down, Left, Right

case class Position(x: Int, y: Int):
  def nextPositionsWithDirections(currentTile: Tile, direction: Direction): Set[(Position, Direction)] =
    currentTile match
      case Tile.Empty     => Set(simple(direction))
      case Tile.LRMirror  => Set(onLRMirror(direction))
      case Tile.RLMirror  => Set(onRLMirror(direction))
      case Tile.VSplitter => onVSplitter(direction)
      case Tile.HSplitter => onHSplitter(direction)

  private def simple(direction: Direction): (Position, Direction) =
    val position = direction match
      case Direction.Up    => Position(x - 1, y)
      case Direction.Down  => Position(x + 1, y)
      case Direction.Left  => Position(x, y - 1)
      case Direction.Right => Position(x, y + 1)
    position -> direction

  private def onVSplitter(direction: Direction): Set[(Position, Direction)] = direction match
    case Direction.Up | Direction.Down    => Set(simple(direction))
    case Direction.Left | Direction.Right => Set(simple(Direction.Up), simple(Direction.Down))

  private def onHSplitter(direction: Direction): Set[(Position, Direction)] = direction match
    case Direction.Left | Direction.Right => Set(simple(direction))
    case Direction.Up | Direction.Down    => Set(simple(Direction.Left), simple(Direction.Right))

  private def onLRMirror(direction: Direction): (Position, Direction) = direction match
    case Direction.Up    => simple(Direction.Right)
    case Direction.Down  => simple(Direction.Left)
    case Direction.Left  => simple(Direction.Down)
    case Direction.Right => simple(Direction.Up)

  private def onRLMirror(direction: Direction): (Position, Direction) = direction match
    case Direction.Up    => simple(Direction.Left)
    case Direction.Down  => simple(Direction.Right)
    case Direction.Left  => simple(Direction.Up)
    case Direction.Right => simple(Direction.Down)

@tailrec
def traverseField(positionsWithDirections: Set[(Position, Direction)], field: Vector[Vector[Tile]])(
  acc: Map[Position, Vector[Direction]]
): Map[Position, Vector[Direction]] =
  positionsWithDirections.headOption match
    case None                        => acc
    case Some((position, direction)) =>
      val maybeCurrentTile = field.lift(position.x).flatMap(_.lift(position.y))
      maybeCurrentTile match
        case None              =>
          traverseField(positionsWithDirections.tail, field)(acc)
        case Some(currentTile) =>
          val uniquePositionsWithDirections = position
            .nextPositionsWithDirections(currentTile, direction)
            .filter((pos, _) => field.lift(pos.x).exists(_.lift(pos.y).isDefined))
            .filter { (pos, dir) =>
              !acc.get(pos).exists(_.contains(dir))
            }
          val updatedAcc                    = uniquePositionsWithDirections.foldLeft(acc) { case (acc, (pos, dir)) =>
            acc.updatedWith(pos) {
              case Some(already) => Some(already :+ dir)
              case None          => Some(Vector(dir))
            }
          }
          traverseField(positionsWithDirections.tail ++ uniquePositionsWithDirections, field)(updatedAcc)

@main def run =
  val field = Source
    .fromResource("day16/task.txt")
    .getLines()
    .toVector
    .map(_.flatMap(Tile.fromString).toVector)

  val startPosition  = Position(0, 0)
  val startDirection = Direction.Right

  val res1 =
    traverseField(Set(startPosition -> startDirection), field)(Map(startPosition -> Vector(startDirection))).values.size

  val allBoundaryTiles: Vector[(Position, Direction)] =
    val columsIndices = field.head.indices.toVector
    val top           = columsIndices.map(Position(0, _) -> Direction.Down)
    val bottom        = columsIndices.map(Position(field.length - 1, _) -> Direction.Up)
    val left          = field.indices.toVector.map(Position(_, 0) -> Direction.Right)
    val right         = field.indices.toVector.map(Position(_, columsIndices.last) -> Direction.Left)

    top ++ bottom ++ left ++ right

  val res2 = allBoundaryTiles
    .map { (position, direction) =>
      traverseField(Set(position -> direction), field)(Map(position -> Vector(direction)))
    }
    .map(_.values.size)
    .max

  println(res1)
  println(res2)
