package com.github.desavitsky.day11

import scala.io.Source

case class Galaxy(x: Int, y: Int):
  def distanceTo(galaxy: Galaxy): Int =
    val res = Math.abs(galaxy.x - x) + Math.abs(galaxy.y - y)
    res

def parseRow(input: String, rowIndex: Int): Vector[Galaxy] =
  input.zipWithIndex.collect {
    case (c, index) if c == '#' =>
      Galaxy(rowIndex, index)
  }.toVector

@main def run =
  val initialGalaxies = Source
    .fromResource("day11/task.txt")
    .getLines()
    .zipWithIndex
    .map(parseRow)
    .toVector

  val rowWithoutGalaxies = initialGalaxies.zipWithIndex
    .collect { case (row, i) if row.isEmpty => i }

  val columsWithGalaxies = initialGalaxies.flatten.distinctBy(_.y).map(_.y)

  val columnsWithoutGalaxies = (0 to columsWithGalaxies.max).diff(columsWithGalaxies)

  def galaxiesAfterExtension(coef: Int): Vector[Galaxy] = initialGalaxies.flatten
    .map { glx =>
      Galaxy(
        glx.x + rowWithoutGalaxies.count(_ < glx.x) * (coef - 1),
        glx.y + columnsWithoutGalaxies.count(_ < glx.y) * (coef - 1)
      )
    }

  val res1 = galaxiesAfterExtension(1)
    .combinations(2)
    .map(pair => pair.head.distanceTo(pair.last))
    .sum

  val res2 = galaxiesAfterExtension(1_000_000)
    .combinations(2)
    .map(pair => pair.head.distanceTo(pair.last))
    .map(_.toLong)
    .sum

  println(res1)
  println(res2)
