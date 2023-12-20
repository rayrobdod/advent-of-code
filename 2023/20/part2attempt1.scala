//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2
//> using dep org.typelevel::toolkit:0.1.20
//> using dep org.typelevel::kittens:3.1.0
//> using file model.scala

/*
 * Had to at least try brute force after part 1
 */
import scala.collection.mutable
import scala.collection.immutable.Queue
import cats.syntax.all.*

def pressButton1(modules: Seq[Module]): (Seq[Module], PulseCounts, Boolean) =
	var state: Map[ModuleName, Module] = modules.groupMapReduce(_.name)(x => x)((_, _) => throw new Exception("more than one module with same name"))
	var counts: PulseCounts = PulseCounts.zero
	var waitingPulses: Queue[Pulse] = Queue(Pulse(PulseLevel.Low, ModuleName(""), ModuleName("broadcaster")))
	var lowPulseWasSentToRx: Boolean = false

	while waitingPulses.nonEmpty do
		val (pulse, nextWaitingPulses) = waitingPulses.dequeue

		//System.out.println(s"ENT: $pulse")
		//System.out.println(s"     $nextWaitingPulses")
		//System.out.println(s"     ${state.view.mapValues(_.typ).toMap}")
		//new java.util.Scanner(System.in).nextLine()

		counts = counts + pulse.level
		if ! state.contains(pulse.sentTo) then
			if pulse.sentTo == ModuleName("rx") && pulse.level == PulseLevel.Low then
				lowPulseWasSentToRx = true
			waitingPulses = nextWaitingPulses
		else
			val receving = state(pulse.sentTo)
			val (nextReceving, newPulses) = receving.receivePulse(pulse.level, pulse.from)
			state = state.updated(pulse.sentTo, nextReceving)
			waitingPulses = nextWaitingPulses ++ newPulses
		end if
	end while

	(state.values.toSeq, counts, lowPulseWasSentToRx)
end pressButton1

def countUntilFirstLowRx(modules: Seq[Module]): Int =
	def rec(state: Seq[Module], count: Int): Int =
		val (next, _, stop) = pressButton1(state)
		if stop then
			count + 1
		else
			rec(next, count + 1)
	rec(modules, 0)
end countUntilFirstLowRx

object Day20Part2:
	def main(args:Array[String]):Unit =
		val modules = parseInput:
			os.read.lines(os.pwd / "input.txt")

		System.out.print("part 2: ")
		System.out.println:
			countUntilFirstLowRx(modules)
