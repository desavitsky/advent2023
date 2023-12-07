package com.github.desavitsky.day7

import math.Ordered.orderingToOrdered
import scala.io.Source

enum Card(val repr: String):
  case Ace   extends Card("A")
  case King  extends Card("K")
  case Queen extends Card("Q")
  case Jack  extends Card("J")
  case Ten   extends Card("T")
  case Nine  extends Card("9")
  case Eight extends Card("8")
  case Seven extends Card("7")
  case Six   extends Card("6")
  case Five  extends Card("5")
  case Four  extends Card("4")
  case Three extends Card("3")
  case Two   extends Card("2")

object Card:

  def from(repr: String): Option[Card] = Card.values.find(_.repr == repr)

  val regularOrdering: Ordering[Card] = (x: Card, y: Card) => -x.ordinal.compare(y.ordinal)

  val jokerOrdering: Ordering[Card] = (x: Card, y: Card) =>
    if x == y then 0
    else if x == Card.Jack then -1
    else if y == Card.Jack then 1
    else -x.ordinal.compare(y.ordinal)

enum HandType:
  case Five, Four, FullHouse, Three, TwoPairs, OnePair, HighCard

object HandType:
  given Ordering[HandType] with
    override def compare(x: HandType, y: HandType): Int = -x.ordinal.compare(y.ordinal)

case class Hand(
  cards: Vector[Card]
):
  val handType: HandType =
    val grouped = cards.groupBy(identity).values.map(_.size).toVector.sortBy(-_)
    if grouped.size == 1 then HandType.Five
    else
      grouped.take(2) match
        case Vector(4, _)   => HandType.Four
        case Vector(3, snd) => if snd == 2 then HandType.FullHouse else HandType.Three
        case Vector(2, snd) => if snd == 2 then HandType.TwoPairs else HandType.OnePair
        case _              => HandType.HighCard

  val handTypeWithJokers: HandType =
    val grouped = cards.filterNot(_ == Card.Jack).groupBy(identity).values.map(_.size).toVector.sortBy(-_)
    if grouped.size == 1 || grouped.isEmpty then HandType.Five
    else
      val jokers = cards.count(_ == Card.Jack)
      grouped.take(2).updated(0, grouped.head + jokers) match
        case Vector(4, _)   => HandType.Four
        case Vector(3, snd) => if snd == 2 then HandType.FullHouse else HandType.Three
        case Vector(2, snd) => if snd == 2 then HandType.TwoPairs else HandType.OnePair
        case _              => HandType.HighCard

object Hand:
  val regularOrgering: Ordering[Hand] = (x: Hand, y: Hand) =>
    given Ordering[Card]   = Card.regularOrdering
    val handTypeComparison = x.handType.compare(y.handType)
    if handTypeComparison != 0 then handTypeComparison
    else x.cards.compare(y.cards)

  val jokerOrgering: Ordering[Hand] = (x: Hand, y: Hand) =>
    given Ordering[Card]   = Card.jokerOrdering
    val handTypeComparison = x.handTypeWithJokers.compare(y.handTypeWithJokers)
    if handTypeComparison != 0 then handTypeComparison
    else x.cards.compare(y.cards)

case class Bid(
  hand: Hand,
  bid: Long
)

object Bid:
  val regularOrdering: Ordering[Bid] = (x: Bid, y: Bid) =>
    given Ordering[Hand] = Hand.regularOrgering
    x.hand.compare(y.hand)

  val jokerOrdering: Ordering[Bid] = (x: Bid, y: Bid) =>
    given Ordering[Hand] = Hand.jokerOrgering
    x.hand.compare(y.hand)

  def fromString(input: String): Bid =
    val split = input.split(" ")
    Bid(
      Hand(
        split.head.flatMap(c => Card.from(c.toString)).toVector
      ),
      split.last.toLong
    )

@main def run =
  val deck = Source
    .fromResource("day7/task.txt")
    .getLines()
    .toVector
    .map(Bid.fromString)

  val res1 = deck
    .sorted(using Bid.regularOrdering)
    .zipWithIndex
    .map { case (bid, place) =>
      bid.bid * (place + 1)
    }
    .sum

  println(res1)

  val debug = deck
    .sorted(using Bid.jokerOrdering)
    .map(bid => bid.hand.cards.map(_.repr).mkString(" ") -> bid.hand.handTypeWithJokers)
    .mkString("\n")

  val res2 = deck
    .sorted(using Bid.jokerOrdering)
    .zipWithIndex
    .map { case (bid, place) =>
      bid.bid * (place + 1)
    }
    .sum

  println(res2)
