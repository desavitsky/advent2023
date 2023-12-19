package com.github.desavitsky.day19

import java.time.Duration
import java.time.Instant
import scala.annotation.tailrec
import scala.io.Source

case class Flow(
  name: String,
  conditionals: Vector[Conditional],
  unconditional: Unconditional
)

object Flow:
  def fromString(str: String): Option[Flow] =
    val name     = str.takeWhile(_ != '{')
    val rulesRaw = str.stripPrefix(name).stripPrefix("{").stripSuffix("}").split(",")
    for {
      unconditional <- Unconditional.unapply(rulesRaw.last)
      conditionals   = rulesRaw.dropRight(1).flatMap(Conditional.unapply)
    } yield Flow(name, conditionals.toVector, unconditional)

enum Comparison(val fun: (Int, Int) => Boolean):
  case Gt extends Comparison(_ > _)
  case Lt extends Comparison(_ < _)

object Comparison:
  def unapply(str: String): Option[Comparison] =
    if str == ">" then Some(Comparison.Gt)
    else if str == "<" then Some(Comparison.Lt)
    else None

sealed trait Result:
  def value: String

case object Accepted extends Result:
  override val value: String = "A"

case object Rejected extends Result:
  override val value: String = "R"

case class PassedDown(direction: String) extends Result:
  override val value: String = direction

object Result:
  def unapply(string: String): Option[Result] =
    if string == "R" then Some(Rejected)
    else if string == "A" then Some(Accepted)
    else Some(PassedDown(string))

sealed trait Rule(val result: Result)

case class Conditional(
  getter: Part => Int,
  comp: Comparison,
  value: Int,
  label: String,
  override val result: Result
) extends Rule(result)

object Conditional:
  private val regex = "([xmas])([<|>])(\\d+):([a-zA-Z]+)".r

  def unapply(str: String): Option[Conditional] = str match
    case regex(field, Comparison(comp), AsInt(value), Result(result)) =>
//      if field == "x" then Some(Conditional(part => comp.fun(part.x, value), result))
      if field == "x" then Some(Conditional(part => part.x, comp, value, "x", result))
      else if field == "m" then Some(Conditional(part => part.m, comp, value, "m", result))
      else if field == "a" then Some(Conditional(part => part.a, comp, value, "a", result))
      else Some(Conditional(part => part.s, comp, value, "s", result))
    case _                                                            => None

case class Unconditional(
  override val result: Result
) extends Rule(result)

object Unconditional:
  def unapply(str: String): Option[Unconditional] =
    Result.unapply(str).map(Unconditional(_))

case class Part(
  x: Int,
  m: Int,
  a: Int,
  s: Int
):
  def sum: Int = x + m + a + s

object Part:
  private val regex = "\\{x=(\\d+),m=(\\d+),a=(\\d+),s=(\\d+)}".r

  def fromString(str: String): Option[Part] = str match
    case regex(AsInt(x), AsInt(m), AsInt(a), AsInt(s)) => Some(Part(x, m, a, s))
    case _                                             => None

object AsInt:
  def unapply(string: String): Option[Int] = string.toIntOption

@tailrec
def runParts(parts: Vector[(Part, Result)], flows: Map[String, Flow])(completed: Long): Long =
  if parts.isEmpty then completed
  else
    val (newCompleted, running) = parts
      .map { (part, in) =>
        part -> flows.get(in.value).map { flow =>
          flow.conditionals
            .find(cond => cond.comp.fun(cond.getter(part), cond.value))
            .getOrElse(flow.unconditional)
            .result
        }
      }
      .collect {
        case (part, Some(result)) if result != Rejected => (part, result)
      }
      .partition(_._2 == Accepted)
    runParts(running, flows)(completed + newCompleted.map(_._1).map(_.sum.toLong).sum)

case class Ranges(
  x: (Int, Int),
  m: (Int, Int),
  a: (Int, Int),
  s: (Int, Int)
):
  def revert(fullPrev: Ranges): Ranges =
    Ranges(
      x = x.revert(fullPrev.x),
      m = m.revert(fullPrev.m),
      a = a.revert(fullPrev.a),
      s = s.revert(fullPrev.s),
    )

  def volume: Long = (x._2 - x._1 + 1).toLong * (m._2 - m._1 + 1) * (a._2 - a._1 + 1) * (s._2 - s._1 + 1)

extension (range: (Int, Int))
  def applyCondition(value: Int, comparison: Comparison): Option[(Int, Int)] =
    if range._1 < value && range._2 > value
    then
      comparison match
        case Comparison.Gt => Some((value + 1, range._2))
        case Comparison.Lt => Some((range._1, value - 1))
    else None

  def revert(full: (Int, Int)): (Int, Int) =
    if range == full then range
    else if range._1 == full._1 then (range._2 + 1, full._2)
    else (full._1, range._1 - 1)

def buildSolutionTree(ranges: Ranges, result: Result)(
  flows: Map[String, Flow]
): Long =
  if result == Accepted then ranges.volume
  else if result == Rejected then 0
  else
    val step = flows(result.value)

    val cond = step.conditionals.collectFirst {
      case cond if cond.label == "x" && ranges.x.applyCondition(cond.value, cond.comp).isDefined =>
        ranges.x.applyCondition(cond.value, cond.comp).map(x => ranges.copy(x = x) -> cond.result)

      case cond if cond.label == "m" && ranges.m.applyCondition(cond.value, cond.comp).isDefined =>
        ranges.m.applyCondition(cond.value, cond.comp).map(m => ranges.copy(m = m) -> cond.result)

      case cond if cond.label == "a" && ranges.a.applyCondition(cond.value, cond.comp).isDefined =>
        ranges.a.applyCondition(cond.value, cond.comp).map(a => ranges.copy(a = a) -> cond.result)

      case cond if cond.label == "s" && ranges.s.applyCondition(cond.value, cond.comp).isDefined =>
        ranges.s.applyCondition(cond.value, cond.comp).map(s => ranges.copy(s = s) -> cond.result)
    }.flatten

    cond
      .map { (condRanges, condResult) =>
        buildSolutionTree(condRanges, condResult)(flows) + buildSolutionTree(
          condRanges.revert(ranges),
          step.unconditional.result
        )(flows)
      }
      .getOrElse {
        buildSolutionTree(ranges, step.unconditional.result)(flows)
      }

@main def run =
  val rawInput = Source
    .fromResource("day19/input.txt")
    .getLines()
    .toVector

  val splitIndex = rawInput.indexWhere(_.isBlank)

  val (flowsRaw, partsRaw) =
    val (left, right) = rawInput.splitAt(splitIndex)
    left -> right.drop(1)

  val flows = flowsRaw.flatMap(Flow.fromString)
  val parts = partsRaw.flatMap(Part.fromString)

  def mapFlowIntoBinary(flow: Flow): Vector[Flow] =
    flow.conditionals.zipWithIndex.map {
      case (conditional, _) if flow.conditionals.size == 1    => flow
      case (conditional, i) if i == 0                         =>
        Flow(
          name = flow.name,
          conditionals = Vector(
            conditional
          ),
          unconditional = Unconditional(PassedDown(flow.name + (i + 1)))
        )
      case (conditional, i) if i < flow.conditionals.size - 1 =>
        Flow(
          name = flow.name + i,
          conditionals = Vector(
            conditional
          ),
          unconditional = Unconditional(PassedDown(flow.name + (i + 1)))
        )
      case (conditional, i)                                   =>
        Flow(
          name = flow.name + i,
          Vector(conditional),
          flow.unconditional
        )
    }

  def mapFlowsToBinaryTree(flows: Vector[Flow]): Vector[Flow] =
    flows.flatMap(mapFlowIntoBinary)

  val res1 = runParts(parts.map(_ -> PassedDown("in")), flows.map(f => f.name -> f).toMap)(0)

  println(res1)

  val res2 = buildSolutionTree(Ranges((1, 4000), (1, 4000), (1, 4000), (1, 4000)), PassedDown("in"))(
    mapFlowsToBinaryTree(flows).map(f => f.name -> f).toMap
  )

  println(res2)
