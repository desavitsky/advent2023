package com.github.desavitsky.day8

import scala.annotation.tailrec
import scala.io.Source

case class Node(
  name: String,
  left: String,
  right: String
)

object Node:
  def fromString(input: String): Option[Node] = input match
    case s"${node} = ($left, $right)" => Some(Node(node, left, right))
    case _                            => None

enum Direction:
  case Left, Right

  override def toString: String = this.productPrefix.take(1)

object Direction:
  def fromString(ch: Char): Option[Direction] =
    if ch == 'L' then Some(Direction.Left)
    else if ch == 'R' then Some(Direction.Right)
    else None

def traverse(start: Node, nodes: Map[String, Node], allDirections: List[Direction])(
  counter: Int,
  directions: List[Direction]
): Int =
  if start.name.endsWith("Z") then counter
  else {
    val (nextStep, remainDirections) = directions match
      case head :: Nil  => (head, allDirections)
      case head :: rest => (head, rest)
      case Nil          => allDirections.head -> allDirections.tail

    val nextNode = nextStep match
      case Direction.Left  => start.left
      case Direction.Right => start.right

    traverse(nodes.getOrElse(nextNode, start), nodes, allDirections)(counter + 1, remainDirections)
  }

@main def run =
  val input = Source
    .fromResource("day8/input.txt")
    .getLines()
    .toVector

  val directions = input.head
    .flatMap(Direction.fromString)
    .toList

  val nodes = input
    .flatMap(Node.fromString)
    .map(n => n.name -> n)
    .toMap

  val startNode = nodes.getOrElse("AAA", throw new IllegalArgumentException("Missing start node"))

  val res1 = traverse(startNode, nodes, directions)(0, directions)

  val startingNodes = nodes.filter(_._1.endsWith("A")).values.toVector

  val res2 = startingNodes
    .map(traverse(_, nodes, directions)(0, directions))
    .map(BigDecimal(_))
    .reduce(lcm)

  println(res1)
  println(res2)

@tailrec
def gcd(number1: BigDecimal, number2: BigDecimal): BigDecimal =
  if (number1 == 0 || number2 == 0) number1 + number2
  else {
    val biggerValue  = number1 max number2
    val smallerValue = number1 min number2
    gcd(biggerValue % smallerValue, smallerValue)
  }

def lcm(number1: BigDecimal, number2: BigDecimal): BigDecimal =
  if (number1 == 0 || number2 == 0) 0
  else {
    val gcdValue = gcd(number1, number2)
    number1 * number2 / gcdValue
  }
