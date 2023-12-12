//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.annotation.tailrec
import scala.collection.immutable.BitSet

sealed trait Condition
sealed trait OptionalCondition
object Operational extends Condition with OptionalCondition:
	override def toString = "."
object Damaged extends Condition with OptionalCondition:
	override def toString = "#"
object Unknown extends OptionalCondition:
	override def toString = "?"

object OptionalCondition:
	def fromChar(c:Char):OptionalCondition =
		c match
			case '#' => Damaged
			case '.' => Operational
			case '?' => Unknown

case class FoldingState1(groups: List[Int], previous: Condition, copies: Long):
	def groupsIsEmpty: Boolean =
		this.groups == Nil || this.groups == List(0)

	def push(current: Condition): Option[FoldingState1] =
		val nextGroups: Option[List[Int]] =
			current match
				case Operational =>
					previous match
						case Operational =>
							Some(groups)
						case Damaged =>
							groups match
								case Nil =>
									None
								case 0 :: tail =>
									Some(tail)
								case _ :: _ =>
									None
				case Damaged =>
					groups match
						case Nil =>
							None
						case head :: tail =>
							Option.when(head - 1 >= 0)((head - 1) :: tail)

		nextGroups.map(FoldingState1(_, current, copies))
	end push

	override def toString: String =
		s"(${groups.mkString("[", ", ", "]")}, '$previous', $copies)"
end FoldingState1

class FoldingState(backing: Seq[FoldingState1]):
	def countOfEmpty: Long =
		backing
			.filter:
				_.groupsIsEmpty
			.map:
				_.copies
			.sum

	def push(current: OptionalCondition): FoldingState =
		//System.out.println(s"ENTERING: $current $backing")
		//new java.util.Scanner(System.in).nextLine()
		// uncomment ^ to step through the fold

		val pushedBacking = current match
			case x: Condition =>
				backing.flatMap(_.push(x))
			case Unknown =>
				backing.flatMap(_.push(Operational)) ++ backing.flatMap(_.push(Damaged))

		// Combine `FoldingState1`s with identical groups and previous to reduce memory pressure
		// and to avoid performing the same computations repeatedly. But mostly the memory pressure.
		// There will be out-of-memory errors without it
		val combinedBacking = 	pushedBacking
			.groupMapReduce
				(x => (x.groups, x.previous))
				(x => x.copies)
				((a, b) => a + b)
			.toSeq
			.map: x =>
				val ((groups, previous), copies) = x
				new FoldingState1(groups, previous, copies)

		new FoldingState(combinedBacking)
	end push

	override def toString: String =
		backing.mkString("FoldingState(", ", ", ")")
end FoldingState

object FoldingState:
	def apply(groups: Seq[Int]): FoldingState =
		new FoldingState(List(new FoldingState1(groups.toList, Operational, 1)))

case class InputLine(eachFormat: Seq[OptionalCondition], groupFormat: Seq[Int]):
	def unfold: InputLine =
		InputLine(
			(1 to 5).flatMap(_ => Unknown +: eachFormat).tail,
			(1 to 5).flatMap(_ => groupFormat),
		)

	def possibleArangements: Long =
		eachFormat
			.foldLeft(FoldingState(groupFormat)): (folding, item) =>
				folding.push(item)
			.countOfEmpty
end InputLine

object Day12:
	def main(args:Array[String]):Unit =
		val input =
			os.read.lines(os.pwd / "input.txt")
				.map:
					case s"$eachStr $groupedStr" =>
						InputLine(
							eachStr.map(OptionalCondition.fromChar),
							groupedStr.split(",").map(_.toInt).toSeq
						)

		val unfoldedInput = input.map:
			_.unfold

		System.out.print("part 1: ")
		System.out.println:
			input
				.map:
					_.possibleArangements
				.sum

		System.out.print("part 2: ")
		System.out.println:
			unfoldedInput
				.map:
					_.possibleArangements
				.sum
