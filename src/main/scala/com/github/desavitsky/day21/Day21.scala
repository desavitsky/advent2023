package com.github.desavitsky.day21

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.io.Source

case class Position(x: Int, y: Int):
  def neighbours: Vector[Position] = Vector(
    copy(x = x + 1),
    copy(x = x - 1),
    copy(y = y - 1),
    copy(y = y + 1)
  )

case class Field(field: Vector[Vector[Char]]):
  val width: Int  = field.head.length
  val height: Int = field.length

  def validPosition(position: Position): Boolean =
    position.x < height && position.y < width && position.x >= 0 && position.y >= 0

  def getPosition(position: Position): Option[Char] =
    field.lift(position.x).flatMap(_.lift(position.y))

  def validForProgression(position: Position): Boolean =
    getPosition(position).exists(s => s == '.' || s == 'S')

  def updatePosition(position: Position, char: Char): Field =
    if validPosition(position) then Field(field.updated(position.x, field(position.x).updated(position.y, char)))
    else this

  override def toString: String = field.map(_.mkString("\t")).mkString("\n")

@tailrec
def step(queue: SortedSet[(Position, Int)])(field: Field)(updField: Field)(step0: Int): Field =
  if queue.isEmpty then updField
  else
    val (position, left) = queue.head
    val neighbours       = position.neighbours.filter(field.validForProgression)
    if left == step0 && left != 0 then
      val upd = neighbours.foldLeft(updField)((field, pos) => field.updatePosition(pos, 'O'))
      step(queue.tail ++ (neighbours.map(_ -> (left - 1))))(field)(upd)(step0)
    else if left != 0 then
      val upd = neighbours.foldLeft(field)((field, pos) => field.updatePosition(pos, 'O'))
      step(queue.tail ++ (neighbours.map(_ -> (left - 1))))(field)(upd)(step0 - 1)
    else step(queue.tail)(field)(updField)(step0)

@main def run =
  val input = Source
    .fromResource("day21/input.txt")
    .getLines()
    .toVector
    .map(_.toVector)

  val field         = Field(input)
  val startPosition = field.field.zipWithIndex
    .map { (row, index) =>
      row.indexWhere(_ == 'S') -> index
    }
    .collectFirst { case (column, row) if column != -1 => Position(row, column) }
    .get

  val steps = 64

  given ord: Ordering[(Position, Int)] with
    override def compare(x: (Position, Int), y: (Position, Int)): Int =
      val ep   = x._2 - y._2
      val difX = x._1.x - y._1.x
      val difY = x._1.y - y._1.y
      if ep == 0 && difX != 0 then difX
      else if ep == 0 then difY
      else ep

  val updField = step(SortedSet[(Position, Int)](startPosition -> steps)(using ord.reverse))(field)(field)(steps)
  val res      = updField.field.map(_.count(c => c == 'O').toLong).sum
  println(res)
