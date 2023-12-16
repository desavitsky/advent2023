package com.github.desavitsky.day13

import java.time.Duration
import java.time.Instant
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

enum MirrorLine(val coeff: Int):
  case Horizontal extends MirrorLine(100)
  case Vertical   extends MirrorLine(1)

def findHorizontalLine(pattern: Vector[String], cond: (String, String) => Boolean): Option[Int] =
  Some {
    val res = pattern.zipWithIndex
      .indexWhere { (raw, index) =>

        val left  = pattern.take(index + 1)
        val right = pattern.slice(index + 1, index + 1 + index + 1)

        cond(
          left.takeRight(left.length min right.length).mkString(""),
          right.take(left.length min right.length).reverse.mkString("")
        )

      }

    res + 1
  }.filter(l => l > 0 && l <= pattern.length - 1)

def findVerticalLine(pattern: Vector[String], cond: (String, String) => Boolean): Option[Int] =
  val patternT = pattern.transpose
  Some {
    val res = patternT.zipWithIndex
      .indexWhere { (raw, index) =>
        val upper  = patternT.take(index + 1)
        val bottom = patternT.drop(index + 1)

        cond(
          upper
            .takeRight(upper.length min bottom.length)
            .map(_.mkString(""))
            .mkString(""),
          bottom
            .take(upper.length min bottom.length)
            .reverse
            .map(_.mkString(""))
            .mkString("")
        )
      }
    res + 1
  }.filter(l => l > 0 && l <= patternT.length - 1)

def find1Diff(string1: String, string2: String): Boolean =

  @tailrec
  def rec(string1: String, string2: String, index: Int, diffs: Int): Boolean =
    if diffs > 1 then false
    else if index >= string1.length then diffs == 1
    else if string1(index) == string2(index) then rec(string1, string2, index + 1, diffs)
    else rec(string1, string2, index + 1, diffs + 1)

  rec(string1, string2, 0, 0)

def findLine(pattern: Vector[String], cond: (String, String) => Boolean): Int =
  findHorizontalLine(pattern, cond)
    .map(_ * 100)
    .orElse(findVerticalLine(pattern, cond))
    .getOrElse {
      println(pattern.mkString("\n"))
      throw new IllegalArgumentException("")
    }

@main def run =
  val input = Source.fromResource("day13/input.txt").getLines().toVector

  val patterns = parseInput(input)(Vector.empty)
  println(patterns.map(findLine(_, _ == _)).sum)
  println(patterns.map(findLine(_, find1Diff)).sum)

@tailrec
def parseInput(vector: Vector[String])(acc: Vector[Vector[String]]): Vector[Vector[String]] =
  val left  = vector.takeWhile(_.nonEmpty)
  val right = vector.drop(left.length + 1)
  if right.nonEmpty then parseInput(right)(acc :+ left)
  else acc :+ left
