package com.github.desavitsky.day4

import scala.io.Source

case class Card(
  id: Int,
  winning: Set[Int],
  numbers: Set[Int]
):
  def points: Int = winning.foldLeft(0) { (points, number) =>
    if (numbers.contains(number))
      Some(points * 2)
        .filter(_ > 0)
        .getOrElse(1)
    else points
  }

  def wonCards: Set[Int] =
    (id + 1 to (id + winning.intersect(numbers).size)).toSet

object AsInt:
  def unapply(str: String): Option[Int] = str.toIntOption

object Card:
  private val regex                           = "Card\\s+(\\d+):([\\s\\d]+)\\|([\\s\\d]+)".r
  def fromString(input: String): Option[Card] =
    input match
      case regex(AsInt(id), winning, numbers) =>
        val winningNumbers = winning.strip.split(" ").flatMap(_.toIntOption).toSet
        val cardNumbers    = numbers.strip.split(" ").flatMap(_.toIntOption).toSet
        Some(Card(id, winningNumbers, cardNumbers))
      case invalid                            =>
        println(s"Invalid card: $invalid")
        None

@main def run =

  val cards = Source
    .fromResource("day4/task.txt")
    .getLines()
    .toVector
    .flatMap(Card.fromString)

  val res1 = cards.map(_.points).sum

  val res2 = cards
    .foldLeft(Map.empty[Int, Int]) { (acc, card) => // acc is how many each cards you have
      val accWithCurrent = Map(card.id -> 1) ++ acc
      val toAdd          = card.wonCards.map(id => id -> accWithCurrent.getOrElse(card.id, 1)).toMap

      accWithCurrent ++ toAdd.map { case (cardId, amount) => cardId -> (amount + accWithCurrent.getOrElse(cardId, 1)) }
    }
    .values
    .sum

  println(res1)
  println(res2)
