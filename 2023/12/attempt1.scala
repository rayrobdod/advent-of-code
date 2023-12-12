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

object Condition:
	def allOfLength(length: Int): Iterable[Seq[Condition]] =
		val LENGTH_WITH_UNIQUE_NAME = length
		new Iterable[Seq[Condition]]():
			def iterator: Iterator[Seq[Condition]] =
				new Iterator[Seq[Condition]]():
					val max:Long = (1 << LENGTH_WITH_UNIQUE_NAME) - 1
					var _next:Long = -1

					def hasNext: Boolean =
						_next < max

					def next: Seq[Condition] =
						_next = _next + 1
						(0 until LENGTH_WITH_UNIQUE_NAME).map: x =>
							if (_next & (1 << x)) == 0 then
								Operational
							else
								Damaged

def matchesEachFormat(format: Seq[OptionalCondition])(self: Seq[Condition]): Boolean =
	self.zip(format).forall:
		case (a, b) if a == b => true
		case (_, Unknown) => true
		case _ => false

def matchesGroupFormat(format: Seq[Int])(self: Seq[Condition]): Boolean =
	def rec(rest: Seq[Condition], format: Seq[Int]): Boolean =
		if rest.isEmpty && format.isEmpty then
			true
		else if rest.isEmpty then
			false
		else if format.isEmpty then
			false
		else
			val damagedCount = rest.takeWhile(_ == Damaged).length
			if damagedCount == format.head then
				rec(rest.dropWhile(_ == Damaged).dropWhile(_ == Operational), format.tail)
			else
				false

	rec(self.dropWhile(_ == Operational), format)

case class InputLine(eachFormat: Seq[OptionalCondition], groupFormat: Seq[Int])

object Day12Part1:
	def main(args:Array[String]):Unit =
		val result =
			os.read.lines(os.pwd / "input.txt")
				.map:
					case s"$eachStr $groupedStr" =>
						InputLine(eachStr.map(OptionalCondition.fromChar), groupedStr.split(",").map(_.toInt).toSeq)
				.map:
					case InputLine(eachFormat, groupFormat) =>
						Condition.allOfLength(eachFormat.length)
							.filter:
								matchesEachFormat(eachFormat)
							.filter:
								matchesGroupFormat(groupFormat)
							.size
				.sum

		// this takes a while even for part 1. It's definitely too slow with the unfolding

		System.out.println(s"part 1: ${result}")

