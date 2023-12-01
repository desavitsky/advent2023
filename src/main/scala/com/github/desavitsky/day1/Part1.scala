package com.github.desavitsky.day1

import scala.io.Source

def cleanString(input: String): Long =
  val cleanedString = input.filter(_.isDigit)
  cleanedString.head.toString.toLong * 10 + cleanedString.last.toString.toLong

def processInput(input: Vector[String]): Long =
  input.foldLeft(0L)(_ + cleanString(_))

@main def run =
  val lines = Source.fromResource("day1/part1.txt").getLines().toVector
  println(processInput(lines))
