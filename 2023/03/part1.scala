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

case class Symbol(pos:Point)

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

class Board private (private val symbols:Set[Point], val numbers:List[Number]):
	def hasSymbolAt(p:Point):Boolean = symbols.contains(p)

	def withSymbol(newVal:Symbol):Board = new Board(symbols + newVal.pos, numbers)
	def withNumber(newVal:Number):Board = new Board(symbols, newVal :: numbers)

	def ++(other:Board):Board = new Board(this.symbols ++ other.symbols, this.numbers ++ other.numbers)

	def numbersAdjacentToSymbols:List[Number] =
		this.numbers.filter({number =>
			//System.out.println(number)
			//System.out.println(number.surroundingPoints & symbols)
			number.surroundingPoints.exists(p => this.hasSymbolAt(p))
		})

	override def toString:String = s"Board(symbols = $symbols, number = $numbers)"

object Board:
	def empty:Board =  new Board(Set.empty, List.empty)
	given Show[Board] = _.toString
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
					case _ => rec(linePart.substring(1), x + 1, retval.withSymbol(Symbol(Point(x, y))))
		rec(line, 0, Board.empty)

	input.zipWithIndex.map(parseLine).combineAll

object Day3Part1 extends IOApp:
	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8Lines(Path("input.txt"))
			.filter(_ != "")
			.compile.toList
			.map(parse)
			//.flatMap(x => IO.println(x).map(_ => x))
			.map({board =>
				board.numbersAdjacentToSymbols
					.map(_.value)
					.sum
			})
			.flatMap(IO.println)
			.map(_ => ExitCode(0))

