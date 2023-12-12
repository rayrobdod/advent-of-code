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

case class InputLine(eachFormat: Seq[OptionalCondition], groupFormat: Seq[Int]):
	def unfold: InputLine =
		InputLine(
			(1 to 5).flatMap(_ => Unknown +: eachFormat).tail,
			(1 to 5).flatMap(_ => groupFormat),
		)

	def possibleArangements: Int =
		def rec(restOfEach: List[OptionalCondition], restOfGroup: List[Int], previous: Condition): Int =
			//System.out.println(s"ENTERING: '${restOfEach.mkString}', $restOfGroup, '$previous'")

			restOfEach match
				case Nil =>
					restOfGroup match
						case Nil => 1
						case 0 :: Nil => 1
						case _ => 0
				case headEach :: tailEach =>
					headEach match
						case Operational =>
							previous match
								case Operational =>
									rec(tailEach, restOfGroup, Operational)
								case Damaged =>
									restOfGroup match
										case Nil =>
											0
										case 0 :: groupTail =>
											rec(tailEach, restOfGroup.tail, Operational)
										case _ :: _ =>
											0
						case Damaged =>
							restOfGroup match
								case Nil =>
									0
								case groupHead :: groupTail =>
									rec(tailEach, (groupHead - 1) :: groupTail, Damaged)
						case Unknown =>
							val value1 = rec(Operational :: tailEach, restOfGroup, previous)
							val value2 = rec(Damaged :: tailEach, restOfGroup, previous)
							value1 + value2

		rec(eachFormat.toList, groupFormat.toList, Operational)

object Day12Part2:
	def main(args:Array[String]):Unit =
		val result =
			os.read.lines(os.pwd / "input.txt")
				.map:
					case s"$eachStr $groupedStr" =>
						InputLine(
							eachStr.map(OptionalCondition.fromChar),
							groupedStr.split(",").map(_.toInt).toSeq
						)
				//.map:
				//	_.unfold
				.map:
					_.possibleArangements
				.sum

		System.out.println(s"part 1: ${result}")

		// part 2 still takes too long with this algorithm

