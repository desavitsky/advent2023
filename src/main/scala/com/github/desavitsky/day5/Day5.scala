package com.github.desavitsky.day5

import concurrent.ExecutionContext.Implicits.global
import java.time.Duration
import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration as SDuration
import scala.io.Source

case class SeedsData(
  seeds: Vector[Long],
  maps: Vector[Vector[MapF]]
):

  def minLocation: Long  = map.min
  def minLocation2: Long = map2.min

  def map: Vector[Long] =
    val totalFunction = maps.foldLeft(identity[Long](_)) { case (fun, mapFs) =>
      mapFs.toFunction compose fun
    }
    seeds.map(totalFunction)

  def map2: Vector[Long] =
    val totalFunction = maps.foldLeft(identity[Long](_)) { case (fun, mapFs) =>
      mapFs.toFunction compose fun
    }
    val result        = Future.sequence(for {
      pair <- seeds.grouped(2).toVector
      res   = Future {
                LazyList
                  .range(pair.head, pair.head + pair.last)
                  .map(totalFunction(_))
                  .grouped(1_000_000)
                  .map(_.min)
                  .min
              }
    } yield res)
    Await.result(result, SDuration.Inf)

case class MapF(destRangeStart: Long, sourceRangeStart: Long, range: Long):
  def value(prev: Long): Long = destRangeStart + prev - sourceRangeStart

  def isInRange(prev: Long): Boolean          = prev >= sourceRangeStart && prev < sourceRangeStart + range
  def toFunction: PartialFunction[Long, Long] = {
    case seed if seed >= sourceRangeStart && seed < sourceRangeStart + range =>
      destRangeStart + seed - sourceRangeStart
  }

object MapF:
  def from(input: Vector[Long]): MapF =
    MapF(input.head, input(1), input.last)

extension (mapFs: Vector[MapF])
  def toFunction: Long => Long = seed =>
    mapFs
      .find(_.isInRange(seed))
      .map(_.value(seed))
      .getOrElse(seed)

@main def run =
  val input =
    Source.fromResource("day5/input.txt").getLines().toVector.foldLeft(SeedsData(Vector.empty, Vector.empty)) {
      case (acc, "")               => acc
      case (acc, s"seeds: $seeds") =>
        acc.copy(seeds = seeds.split(" ").map(_.toLong).toVector)
      case (acc, s"${_} map:")     =>
        acc.copy(
          maps = acc.maps.appended(Vector.empty)
        )
      case (acc, numbers)          =>
        acc.copy(
          maps = acc.maps
            .updated(
              acc.maps.length - 1,
              acc
                .maps(acc.maps.length - 1)
                .appended(
                  MapF.from(
                    numbers
                      .split(" ")
                      .map(_.toLong)
                      .toVector
                  )
                )
            )
        )
    }
  //  println(input)
  println(input.minLocation)
  println(input.minLocation2)
