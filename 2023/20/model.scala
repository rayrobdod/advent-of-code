import scala.collection.mutable
import scala.collection.immutable.Queue
import cats.syntax.all.*

case class PulseCounts(low: Long, high: Long):
	def +(level: PulseLevel): PulseCounts =
		import PulseLevel.*
		level match
			case High => PulseCounts(low, high + 1)
			case Low => PulseCounts(low + 1, high)

	def product: Long = low * high
end PulseCounts

object PulseCounts:
	given cats.Monoid[PulseCounts] = cats.derived.semiauto.monoid

	def zero:PulseCounts = cats.Monoid[PulseCounts].empty
end PulseCounts

enum PulseLevel:
	case High, Low

	def unary_! :PulseLevel =
		this match
			case High => Low
			case Low => High

type ModuleName = ModuleName.Type
object ModuleName:
	opaque type Type = String
	inline def apply(s:String):ModuleName = s

case class Pulse(level: PulseLevel, from: ModuleName, sentTo: ModuleName)

enum ModuleType:
	case Broadcast
	case FlipFlop(state: PulseLevel)
	case Conjunction(inputs: Map[ModuleName, PulseLevel])

	def receivePulse(level: PulseLevel, from: ModuleName): (ModuleType, Option[PulseLevel]) =
		this match
			case Broadcast => ((this, Some(level)))
			case FlipFlop(state) =>
				level match
					case PulseLevel.High => ((this, None))
					case PulseLevel.Low => ((FlipFlop(!state), Some(!state)))
			case Conjunction(state) =>
				val newState = state.updated(from, level)
				val outLevel = if newState.values.forall(_ == PulseLevel.High) then PulseLevel.Low else PulseLevel.High
				((Conjunction(newState), Some(outLevel)))
end ModuleType
import ModuleType.{Broadcast, FlipFlop, Conjunction}

case class Module(val name: ModuleName, val destinations: Set[ModuleName], typ: ModuleType):
	def receivePulse(level: PulseLevel, from: ModuleName): (Module, Set[Pulse]) =
		val (newTyp, pulseLevel) = this.typ.receivePulse(level, from)
		val sents = pulseLevel match
			case None => Set.empty
			case Some(level) => destinations.map(dst => Pulse(level, name, dst))
		((Module(name, destinations, newTyp), sents))
end Module

def parseInput(lines: Seq[String]): Seq[Module] =
    val lines2 = lines
      .map:
        case s"broadcaster -> $dests" =>
          (("broadcaster", dests, Broadcast))
        case s"%$name -> $dests" =>
          ((name, dests, FlipFlop(PulseLevel.Low)))
        case s"&$name -> $dests" =>
          ((name, dests, Conjunction(Map.empty)))
      .map: (name, destsStr, typ) =>
        Module(
          ModuleName(name),
          destsStr.split(", ").map(x => ModuleName(x)).toSet,
          typ,
        )
    val inputs = lines2
      .flatMap: line =>
        val Module(from, tos, _) = line
        tos.map(to => to -> from)
      .groupMap(_._1)(_._2)

    val (conjunctions, rest) = lines2.partitionMap:
      case mod @ Module(_, _, ModuleType.Conjunction(_)) => Left(mod)
      case mod => Right(mod)

    val conjunctions2 = conjunctions.map: line =>
      val Module(from, tos, _) = line
      Module(from, tos, Conjunction(inputs(from).map(input => input -> PulseLevel.Low).toMap))
    val lines3 = conjunctions2 ++ rest
    lines3
