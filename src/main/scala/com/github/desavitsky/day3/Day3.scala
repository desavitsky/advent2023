package com.github.desavitsky.day3

import scala.annotation.tailrec
import scala.io.Source

def searchSymbolsInSurrounding(input: Vector[String], rowIndex: Int, index: Int, numberLength: Int): Boolean =
  def cond(row: String): Boolean =
    row
      .substring(
        Some(index - 1).filter(_ >= 0).getOrElse(0),
        Some(index + numberLength + 1).filter(_ < row.length).getOrElse(row.length - 1)
      )
      .exists(char => !(char.isDigit || char == '.'))

  input.lift(rowIndex - 1).exists(cond) ||
  input.lift(rowIndex).exists(cond) ||
  input.lift(rowIndex + 1).exists(cond)

def productInGearSurrounding(input: Vector[String], rowIndex: Int, gearIndex: Int): Int =
  def numbers(row: String): Vector[Int] =
    numbersWithIndexInRow(row)
      .collect {
        case (numStr, index) if gearIndex >= index - 1 && gearIndex <= numStr.length + index =>
          numStr.toInt
      }

  val numbersWithIndexPrevious = input
    .lift(rowIndex - 1)
    .map(numbers)
    .getOrElse(Vector.empty)
  val numbersWithIndexCurrent  = input
    .lift(rowIndex)
    .map(numbers)
    .getOrElse(Vector.empty)
  val numbersWithIndexNext     = input
    .lift(rowIndex + 1)
    .map(numbers)
    .getOrElse(Vector.empty)

  val allNumbers = numbersWithIndexPrevious ++ numbersWithIndexCurrent ++ numbersWithIndexNext
  if allNumbers.size == 2 then allNumbers.product
  else 0

type Index = Int
def numbersWithIndexInRow(row: String): Vector[(String, Index)] =

  @tailrec
  def numbersWithIndexInRowStartingOn(row: String, startIndex: Int)(
    acc: Vector[(String, Index)]
  ): Vector[(String, Index)] =
    Some(row.substring(startIndex).indexWhere(_.isDigit)).filter(_ != -1) match
      case Some(index) =>
        val numberStr  = row.substring(startIndex + index).takeWhile(_.isDigit)
        val updatedAcc = acc :+ (numberStr, startIndex + index)
        val newIndex   = index + startIndex + numberStr.length
        if newIndex >= row.length then updatedAcc
        else numbersWithIndexInRowStartingOn(row, newIndex)(updatedAcc)
      case None        => acc

  numbersWithIndexInRowStartingOn(row, 0)(Vector.empty)

def part1(input: Vector[String]): Int =
  input.zipWithIndex
    .foldLeft(0) { case (sum, (row, rowIndex)) =>
      val numbersWithIndex = numbersWithIndexInRow(row)
      numbersWithIndex.collect {
        case (str, index) if searchSymbolsInSurrounding(input, rowIndex, index, str.length) =>
          str.toInt
      }.sum + sum
    }

def part2(input: Vector[String]): Int =
  input.zipWithIndex
    .foldLeft(0) { case (sum, (row, rowIndex)) =>
      val gearIndexes = row.zipWithIndex.collect { case ('*', index) => index }
      if gearIndexes.nonEmpty then
        sum + gearIndexes
          .map(productInGearSurrounding(input, rowIndex, _))
          .sum
      else sum
    }

@main def run =
  val input = Source.fromResource("day3/input.txt").getLines().toVector
  println(part1(input))
  println(part2(input))
