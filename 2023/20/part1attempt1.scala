//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2
//> using dep org.typelevel::toolkit:0.1.20
//> using dep org.typelevel::kittens:3.1.0
//> using file model.scala

/*
 * Cycle detection does not work when the input does not cycle
 */

import scala.collection.mutable
import scala.collection.immutable.Queue
import cats.syntax.all.*

def pressButton1(modules: Seq[Module]): (Seq[Module], PulseCounts) =
	var state: Map[ModuleName, Module] = modules.groupMapReduce(_.name)(x => x)((_, _) => throw new Exception("more than one module with same name"))
	var counts: PulseCounts = PulseCounts.zero
	var waitingPulses: Queue[Pulse] = Queue(Pulse(PulseLevel.Low, ModuleName(""), ModuleName("broadcaster")))

	while waitingPulses.nonEmpty do
		val (pulse, nextWaitingPulses) = waitingPulses.dequeue

		//System.out.println(s"ENT: $pulse")
		//System.out.println(s"     $nextWaitingPulses")
		//System.out.println(s"     ${state.view.mapValues(_.typ).toMap}")
		//new java.util.Scanner(System.in).nextLine()

		counts = counts + pulse.level
		if ! state.contains(pulse.sentTo) then
			waitingPulses = nextWaitingPulses
		else
			val receving = state(pulse.sentTo)
			val (nextReceving, newPulses) = receving.receivePulse(pulse.level, pulse.from)
			state = state.updated(pulse.sentTo, nextReceving)
			waitingPulses = nextWaitingPulses ++ newPulses
		end if
	end while

	(state.values.toSeq, counts)
end pressButton1

def pressButton1000(modules: Seq[Module]): (Seq[Module], PulseCounts) =
	val TOTAL_CYCLES = 1000
	val seen = mutable.Buffer.empty[(Seq[Module], PulseCounts)]
	var current = (modules, PulseCounts.zero)

	while ! seen.contains(current) do
		seen += current
		current = pressButton1(current._1)

	val currentIndex = seen.length
	val cycleStart = seen.indexOf(current)
	val cycleLength = seen.length - cycleStart

	val prefixPulseCounts = cats.Monoid[PulseCounts].combineAll(seen.drop(1).take(cycleStart).map(_._2))

	val cyclePulseCounts = cats.Monoid[PulseCounts].combineAll(seen.drop(cycleStart).map(_._2))
	val remaining = (TOTAL_CYCLES - currentIndex) % cycleLength
	val cycleRepeats = (TOTAL_CYCLES - cycleStart - remaining) / cycleLength
	val cycleAllPulseCounts = cyclePulseCounts.combineN(cycleRepeats)

	val (endState, suffixPulseCounts) = (0 until remaining)
		.foldLeft((current._1, PulseCounts.zero)): (folding, _) =>
			val (state, accCounts) = folding
			val (next, nextCounts) = pressButton1(state)
			(next, accCounts |+| nextCounts)

	(endState, prefixPulseCounts |+| cycleAllPulseCounts |+| suffixPulseCounts)
end pressButton1000

object Day20Part1:
	def main(args:Array[String]):Unit =
		val modules = parseInput:
			os.read.lines(os.pwd / "sample.txt")

		System.out.print("part 1: ")
		System.out.println:
			pressButton1000(modules)._2.product
