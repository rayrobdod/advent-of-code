//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2
//> using dep org.typelevel::toolkit:0.1.20
//> using dep org.typelevel::kittens:3.1.0
//> using file model.scala

/*
 * My input has
 *
 * ```
 * &mp -> dr
 * &qt -> dr
 * &qb -> dr
 * &ng -> dr
 *
 * &dr -> rx
 * ```

 * with no other modules sending pulses to either of `dr` or `rx`.
 *
 * Therefore, `rx` recieves a Low pulse when all four of `mp`, `qt`, `qb` and `ng` recieve a Low pulse
 *
 * There is no reason to assume that the low pulses to `mp`, `qt`, `qb` and `ng`
 * occur with a regular cycle of one every N button presses starting with 0,
 * but there was also no reason to assume that during day 8 either.
 *
 * Anyway, assume GCD of the first low pulse to these four modules is the low first pulse of `rx`,
 * maybe check a few more button presses to eyeball how cyclical the module's low pulses are.
 */

import scala.collection.mutable
import scala.collection.immutable.Queue
import cats.syntax.all.*

def pressButton1(modules: Seq[Module], checkRecvLow: Set[ModuleName]): (Seq[Module], Set[ModuleName]) =
	var state: Map[ModuleName, Module] = modules.groupMapReduce(_.name)(x => x)((_, _) => throw new Exception("more than one module with same name"))
	var waitingPulses: Queue[Pulse] = Queue(Pulse(PulseLevel.Low, ModuleName(""), ModuleName("broadcaster")))
	var lowPulseWasSent: Set[ModuleName] = Set.empty

	while waitingPulses.nonEmpty do
		val (pulse, nextWaitingPulses) = waitingPulses.dequeue

		if pulse.level == PulseLevel.Low && checkRecvLow.contains(pulse.sentTo) then
			lowPulseWasSent = lowPulseWasSent + pulse.sentTo

		if ! state.contains(pulse.sentTo) then
			waitingPulses = nextWaitingPulses
		else
			val receving = state(pulse.sentTo)
			val (nextReceving, newPulses) = receving.receivePulse(pulse.level, pulse.from)
			state = state.updated(pulse.sentTo, nextReceving)
			waitingPulses = nextWaitingPulses ++ newPulses
		end if
	end while

	(state.values.toSeq, lowPulseWasSent)
end pressButton1

def countUntilFirstLows(modules: Seq[Module], destinations: Set[ModuleName]): Map[ModuleName, Int] =
	def rec(state: Seq[Module], press:Int, results: Map[ModuleName, Int]): Map[ModuleName, Int] =
		val nextPress = press + 1
		val (next, lowPulses) = pressButton1(state, destinations)
		/*
		lowPulses.foreach: lowPulse =>
			System.out.print(s"$lowPulse -> $nextPress")
			if results.contains(lowPulse) then
				System.out.print(s" (${nextPress.toDouble / results(lowPulse)})")
			System.out.println()
		*/

		val additionalResults = lowPulses
			.filterNot:
				results.contains
			.map: name =>
				name -> nextPress
			.toMap

		val nextResults: Map[ModuleName, Int] = results ++ additionalResults

		//if press >= 40000 then
		if destinations.sizeIs == nextResults.size then
			nextResults
		else
			rec(next, nextPress, nextResults)
	rec(modules, 0, Map.empty)
end countUntilFirstLows

object Day20Part2:
	def main(args:Array[String]):Unit =
		val modules = parseInput:
			os.read.lines(os.pwd / "input.txt")

		val Target = ModuleName("rx")

		// assumes `&dr -> rx` and a set of `&nn -> dr` with no other `-> rx` or `-> dr` =
		val TargetInputInputs = modules
			.collect:
				case Module(_, dests, ModuleType.Conjunction(inputs)) if dests == Set(Target) =>
					inputs.keySet
			.reduce((_, _) => throw new Exception("more than one target input"))

		val parts = countUntilFirstLows(modules, TargetInputInputs)
		System.out.println(parts)
		// all four values are prime.

		System.out.print("part 2: ")
		System.out.println:
			parts.values.map(_.toLong).product
