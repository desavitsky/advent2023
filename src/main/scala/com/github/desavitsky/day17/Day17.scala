package com.github.desavitsky.day17

import Direction.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

case class Position(x: Int, y: Int)

object Position:
  def neighbours(position: Position): Vector[(Position, Direction)] =
    Vector(
      position.copy(x = position.x + 1) -> Down,
      position.copy(x = position.x - 1) -> Up,
      position.copy(y = position.y - 1) -> Left,
      position.copy(y = position.y + 1) -> Right,
    )

def distances(field: Vector[Vector[Int]]): State =

  @tailrec
  def rev(verticesToVisit: mutable.PriorityQueue[State])(
    visitedNodes: Set[Node]
  ): State =
    verticesToVisit.headOption match
      case None =>
        println("WTF")
        ???
      case Some(state@State(node@Node(position, _, steps), _)) =>
        if field.lift(position.x).flatMap(_.lift(position.y)).isEmpty then rev(verticesToVisit.tail)(visitedNodes)
        else if visitedNodes.contains(node) then rev(verticesToVisit.tail)(visitedNodes)
        else if position.x == field.length - 1 && position.y == field.head.length - 1 && steps < 3 then
          rev(verticesToVisit.tail)(visitedNodes)
        else if position.x == field.length - 1 && position.y == field.head.length - 1 then
          state
        else
          val toVisit: Vector[State] = state.nextStates2(field)
          rev(verticesToVisit ++ toVisit)(visitedNodes + node)

  rev(
    mutable.PriorityQueue(
      State(
        Node(Position(0, 1), Right, 1),
        field(0)(1)
      ),
      State(
        Node(Position(1, 0), Down, 1),
        field(1)(0)
      ),
    )(using Ordering[State].reverse)
  )(Set.empty)


case class State(
  node: Node,
  cost: Int
):
  def nextStates(field: Vector[Vector[Int]]): Vector[State] =
    Position
      .neighbours(node.position)
      .filter(_._2 != node.direction.opposite)
      .collect {
        case (pos, dir)
            if (node.steps < 2 || dir !=node.direction) && field.lift(pos.x).flatMap(_.lift(pos.y)).isDefined =>
          State(
            Node(
              pos,
              dir,
              if dir == node.direction then node.steps + 1 else 0
            ),
            cost + field(pos.x)(pos.y)
          )
      }

  def nextStates2(field: Vector[Vector[Int]]): Vector[State] =
    Position
      .neighbours(node.position)
      .filter(_._2 != node.direction.opposite)
      .collect {
        case (pos, dir) if node.steps < 3 && field.lift(pos.x).flatMap(_.lift(pos.y)).isDefined && node.direction == dir =>
          State(
            Node(
              pos,
              dir,
              node.steps + 1
            ),
            cost + field(pos.x)(pos.y)
          )
        case (pos, dir)
          if node.steps >= 3 && (node.steps < 9 || dir !=node.direction) && field.lift(pos.x).flatMap(_.lift(pos.y)).isDefined =>
          State(
            Node(
              pos,
              dir,
              if dir == node.direction then node.steps + 1 else 0
            ),
            cost + field(pos.x)(pos.y)
          )
      }

object State:
  given Ordering[State] with
    override def compare(x: State, y: State): Int =
      val cost = x.cost - y.cost
      if cost == 0 then x.node.position.x + x.node.position.y - y.node.position.x - y.node.position.y
      else cost

case class Node(
  position: Position,
  direction: Direction,
  steps: Int
)

@main def run =
  val input = Source
    .fromResource("day17/input.txt")
    .getLines()
    .toVector
    .map(_.map(_.toString.toInt).toVector)

  println(input.length)
  println(input.head.length)

  val res2 = distances(input)
