package com.github.desavitsky.day20

import com.github.desavitsky.day20.Pulse.*
import com.github.desavitsky.day20.State.On
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source

enum Pulse:
  case High, Low

enum State:
  case On, Off

sealed trait Module(val name: String, val sendTo: Vector[String]):
  def applyPulse(from: String, pulse: Pulse): (Option[Pulse], Module)

object Module:
  case class BroadCast(override val sendTo: Vector[String]) extends Module("broadcaster", sendTo):

    override def applyPulse(from: String, pulse: Pulse): (Option[Pulse], Module) = (Some(Low), this)

  case class FLipFlop(override val name: String, state: State, override val sendTo: Vector[String])
      extends Module(name, sendTo):
    override def applyPulse(from: String, pulse: Pulse): (Option[Pulse], Module) =
      pulse match
        case Pulse.High => (None, this)
        case Pulse.Low  =>
          if state == State.Off then Some(Pulse.High) -> copy(state = On)
          else (Some(Pulse.Low), copy(state = State.Off))

  case class Conjunction(override val name: String, history: Map[String, Pulse], override val sendTo: Vector[String])
      extends Module(name, sendTo):
    override def applyPulse(from: String, pulse: Pulse): (Option[Pulse], Module) =
      val updatedHistory   = history.updated(from, pulse)
      val updatedCondition = copy(history = updatedHistory)
      if updatedCondition.history.forall(_._2 == Pulse.High) then (Some(Pulse.Low), updatedCondition)
      else (Some(Pulse.High), updatedCondition)

  def parse(input: String): Option[Module] = input match
    case s"&$name -> $sendTo"      => Some(Conjunction(name, Map.empty, sendTo.split(",").map(_.strip()).toVector))
    case s"%$name -> $sendTo"      => Some(FLipFlop(name, State.Off, sendTo.split(",").map(_.strip()).toVector))
    case s"broadcaster -> $sendTo" => Some(BroadCast(sendTo.split(",").map(_.strip()).toVector))
    case _                         => None

val preHp = Set("sn", "sr", "vq", "rf")

@tailrec
def loopModules(modules: Map[String, Module])(pulseQ: Queue[(String, String, Pulse)])(
  left: Int
)(lowAcc: Long, highAcc: Long): (Long, Long) =
  if pulseQ.isEmpty && left > 0 then
    loopModules(modules)(pulseQ.appended(("button", "broadcaster", Pulse.Low)))(left - 1)(lowAcc + 1, highAcc)
  else if pulseQ.isEmpty then (lowAcc, highAcc)
  else
    val ((from, to, signal), queue) = pulseQ.dequeue
    modules.get(to) match
      case Some(module) =>
        val (maybeSignal, updModule) = module.applyPulse(from, signal)
        maybeSignal match
          case Some(outSignal) =>
            val (updLow, updHigh) = outSignal match
              case Pulse.High => (lowAcc, highAcc + updModule.sendTo.size)
              case Pulse.Low  => (lowAcc + updModule.sendTo.size, highAcc)
            loopModules(modules.updated(to, updModule))(
              queue.appendedAll(
                updModule.sendTo.map((to, _, outSignal))
              )
            )(left)(updLow, updHigh)
          case None            => loopModules(modules)(queue)(left)(lowAcc, highAcc)
      case None         =>
        loopModules(modules)(queue)(left)(lowAcc, highAcc)

@tailrec
def stopWhenModuleSendsHighTwice(modules: Map[String, Module], moduleName: String)(
  pulseQ: Queue[(String, String, Pulse)]
)(number: Int)(
  count: Vector[Int]
): Int =
  if pulseQ.isEmpty && count.size == 2 then count.last - count.head
  else if pulseQ.isEmpty then
    stopWhenModuleSendsHighTwice(modules, moduleName)(pulseQ.appended(("button", "broadcaster", Pulse.Low)))(
      number + 1
    )(count)
  else
    val ((from, to, signal), queue) = pulseQ.dequeue
    modules.get(to) match
      case Some(module) =>
        val (maybeSignal, updModule) = module.applyPulse(from, signal)
        maybeSignal match
          case Some(outSignal) =>
            val updCount = if from == moduleName && signal == Pulse.High then count :+ number else count
            stopWhenModuleSendsHighTwice(modules.updated(to, updModule), moduleName)(
              queue.appendedAll(
                updModule.sendTo.map((to, _, outSignal))
              )
            )(number)(updCount)
          case None            => stopWhenModuleSendsHighTwice(modules, moduleName)(queue)(number)(count)
      case None         =>
        stopWhenModuleSendsHighTwice(modules, moduleName)(queue)(number)(count)
@main def run =
  val preModules = Source
    .fromResource("day20/input.txt")
    .getLines()
    .toVector
    .flatMap(Module.parse)

  val readyModules = preModules.map {
    case Module.Conjunction(name, history, sendTo) =>
      val senders = preModules.collect { case module if module.sendTo.contains(name) => module.name }
      Module.Conjunction(name, senders.map(_ -> Pulse.Low).toMap, sendTo)
    case other                                     => other
  }

  val result = loopModules(readyModules.map(m => m.name -> m).toMap)(Queue.empty)(1000)(0, 0)
  val res1   = result._1 * result._2
  println(res1)

  val requiredModules = readyModules
    .find(_.sendTo.contains("rx"))
    .collect { case Module.Conjunction(name, history, sendTo) => history.keySet }
    .getOrElse(Set.empty)

  val res2 = requiredModules.map {
    stopWhenModuleSendsHighTwice(readyModules.map(m => m.name -> m).toMap, _)(Queue.empty)(1)(Vector.empty).toLong
  }.product

  println(res2)
