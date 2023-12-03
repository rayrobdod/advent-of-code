//> using dep org.typelevel::toolkit:0.1.19

import scala.annotation.tailrec
import cats.Show
import cats.syntax.all._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path

case class Point(x:Int, y:Int):
	def +(dx:Int, dy:Int):Point =
		Point(x + dx, y + dy)

case class GearSuspect(pos:Point)

case class Number(leftPos:Point, width:Int, value:Int):
	def surroundingPoints:Set[Point] =
		(
			for (
				dy <- (-1 to 1);
				dx <- (-1 to width)
			) yield {
				leftPos.+(dx, dy)
			}
		).toSet

class Board private (private val gearSuspect:Set[Point], val numbers:List[Number]):
	def hasSymbolAt(p:Point):Boolean = gearSuspect.contains(p)

	def withGearSuspect(newVal:GearSuspect):Board = new Board(gearSuspect + newVal.pos, numbers)
	def withNumber(newVal:Number):Board = new Board(gearSuspect, newVal :: numbers)

	def ++(other:Board):Board = new Board(this.gearSuspect ++ other.gearSuspect, this.numbers ++ other.numbers)

	def gearRatios: Seq[Int] =
		gearSuspect.toSeq
			.map(p => numbers.filter(_.surroundingPoints.contains(p)))
			.collect({case a :: b :: Nil => a.value * b.value})


object Board:
	def empty:Board =  new Board(Set.empty, List.empty)
	given cats.Monoid[Board] with
		def combine(left:Board, right:Board):Board = left ++ right
		def empty:Board = Board.empty

def parse(input:Seq[String]):Board =
	def parseLine(lineIndex:(String, Int)):Board =
		val (line, y) = lineIndex

		@tailrec def rec(linePart: String, x: Int, retval: Board): Board =
			if 0 == linePart.length then
				return retval
			else
				linePart.charAt(0) match
					case '.' => rec(linePart.substring(1), x + 1, retval)
					case char if char.isDigit => {
						val valueStr = linePart.takeWhile(_.isDigit)
						val valueLen = valueStr.length
						val newValue = Number(Point(x,y), valueLen, valueStr.toInt)
						rec(linePart.substring(valueLen), x + valueLen, retval.withNumber(newValue))
					}
					case '*' => rec(linePart.substring(1), x + 1, retval.withGearSuspect(GearSuspect(Point(x, y))))
					case _ => rec(linePart.substring(1), x + 1, retval)
		rec(line, 0, Board.empty)

	input.zipWithIndex.map(parseLine).combineAll

object Day3Part2 extends IOApp:
	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8Lines(Path("input.txt"))
			.filter(_ != "")
			.compile.toList
			.map(parse)
			.map({board => board.gearRatios.sum})
			.flatMap(IO.println)
			.map(_ => ExitCode(0))

