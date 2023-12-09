package com.github.desavitsky.day9

import scala.io.Source

def extrapolateSeq(seq: Seq[Int]): Seq[Int] =
  val newSeq = seq.sliding(2).map(s => s.last - s.head).toVector
  if !newSeq.forall(_ == 0) then
    val extrapolatedNext = extrapolateSeq(newSeq)
    (seq.head - extrapolatedNext.head) +: seq :+ (seq.last + extrapolatedNext.last)
  else seq ++ seq.take(2)

@main def run =
  val input = Source
    .fromResource("day9/task.txt")
    .getLines()
    .toVector
    .map(_.split("\\s+").flatMap(_.toIntOption).toVector)

  val res1 = input
    .map(extrapolateSeq)
    .map(_.last)
    .sum

  val res2 = input
    .map(extrapolateSeq)
    .map(_.head)
    .sum

  println(res1)
  println(res2)
