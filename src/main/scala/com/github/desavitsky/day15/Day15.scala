package com.github.desavitsky.day15

import scala.collection.immutable.ListMap
import scala.io.Source

def hashcode(str: String): Int =

  def updateHash(prevValue: Int, char: Char): Int =
    (prevValue + char.toInt) * 17 % 256

  str.foldLeft(0)(updateHash)

def processBoxes(current: String)(boxes: Map[Int, ListMap[String, Int]]): Map[Int, ListMap[String, Int]] =
  if current.endsWith("-") then
    val name = current.dropRight(1)
    boxes.updatedWith(hashcode(name))(_.map(_.removed(name)))
  else
    val split  = current.split("=")
    val name   = split.head
    val amount = split.last.toInt
    boxes.updatedWith(hashcode(name)) {
      case Some(listMap) => Some(listMap + (name -> amount))
      case None          => Some(ListMap(name -> amount))
    }

@main def run =
  val input = Source
    .fromResource("day15/input.txt")
    .getLines()
    .toVector
    .head
    .split(",")

  val res1 = input
    .map(hashcode)
    .sum

  val res2 = input
    .foldLeft(Map.empty[Int, ListMap[String, Int]])((acc, str) => processBoxes(str)(acc))
    .map { (boxN, list) =>
      (boxN + 1) * list.zipWithIndex.map { case ((_, fl), index) => fl * (index + 1) }.sum
    }
    .sum

  println(res1)
  println(res2)
