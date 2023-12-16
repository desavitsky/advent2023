package com.github.desavitsky.day1

import scala.io.Source

enum Number:
  case Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine

  val replace: String = s"${this.productPrefix.head.toLower}${this.ordinal}${this.productPrefix.last}"

def cleanString2(input: String): Long =
  val changedStr = Number.values.foldLeft(input) { (mod, nmb) =>
    println(nmb.productPrefix)
    mod.replace(nmb.productPrefix.toLowerCase, nmb.replace)
  }
  cleanString(changedStr)

def processInput2(input: Vector[String]): Long =
  input.foldLeft(0L)(_ + cleanString2(_))

@main def run2 =
  val lines = Source.fromResource("day1/input.txt").getLines().toVector
  println(processInput2(lines))
