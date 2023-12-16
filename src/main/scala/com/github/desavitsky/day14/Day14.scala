package com.github.desavitsky.day14

import scala.annotation.tailrec
import scala.io.Source

def processColumn(column: Vector[Char]): Vector[Char] =
  val cubes  = column.zipWithIndex.collect { case ('#', index) => index }
  val rounds = column.zipWithIndex.collect { case ('O', index) => index }

  @tailrec
  def move(index: Int, rounds: Vector[Int], cubes: Vector[Int])(acc: Vector[Char]): Vector[Char] =
    if index >= column.length then acc
    else if rounds.isEmpty then acc ++ Vector.fill(column.length - acc.length)('.')
    else if cubes.isEmpty then
      (acc ++ Vector.fill(rounds.length)('O') ++ Vector.fill(column.length - acc.length)('.')).take(column.length)
    else if index == cubes.head then move(index + 1, rounds, cubes.tail)(acc :+ '#')
    else
      val nextCubeCoord = cubes.head
      val roundsToPut   = rounds.takeWhile(_ < nextCubeCoord).size
      if roundsToPut == 0 then move(nextCubeCoord, rounds, cubes)(acc ++ Vector.fill(nextCubeCoord - index)('.'))
      else
        move(nextCubeCoord, rounds.drop(roundsToPut), cubes)(
          acc ++ Vector.fill(roundsToPut)('O') ++ Vector.fill(nextCubeCoord - index - roundsToPut)('.')
        )

  val notCompleteResult = move(0, rounds, cubes)(Vector.empty)
  cubes.foldLeft(notCompleteResult) { case (res, cubeIndex) =>
    res.patch(cubeIndex, List('#'), 1)
  }

@tailrec
def loop(table: Vector[Vector[Char]], n: Int, current: Int, cache: Map[Int, Int]): Vector[Vector[Char]] =
  if n == current then table
  else {
    val north      = table.map(processColumn)
    val west       = north.transpose.map(processColumn)
    val south      = west.transpose.map(c => processColumn(c.reverse).reverse)
    val east       = south.transpose.map(c => processColumn(c.reverse).reverse)
    val transposed = east.transpose
    val hc         = transposed.hashCode()
    if (cache.contains(hc)) {
      val nextIndex =
        current + (n - current) / (current - cache.getOrElse(hc, 0)) * (current - cache.getOrElse(hc, 0))
      if nextIndex == current then loop(transposed, n, current + 1, cache)
      else loop(transposed, n, nextIndex, cache + (hc -> current))
    } else
      loop(transposed, n, current + 1, cache + (hc -> current))
  }

@main def run =

  val input = Source
    .fromResource("day14/input.txt")
    .getLines()
    .toVector
    .transpose

  val res0Part1 = input.map(processColumn).transpose

  val res1Part1 = res0Part1
    .lazyZip(res0Part1.size to (1, -1))
    .map { case (chars, index) =>
      chars.count(_ == 'O') * index
    }
    .sum

  println(res1Part1)

  val res0Part2 = loop(input, 1_000_000_000, 1, Map.empty).transpose

  val res1Part2 = res0Part2
    .lazyZip(res0Part2.size to (1, -1))
    .map { case (chars, index) =>
      chars.count(_ == 'O') * index
    }
    .sum

  println(res1Part2)
