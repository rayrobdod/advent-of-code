//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2
//> using file ../Grid.scala

import scala.annotation.tailrec
import name.rayrobdod.aoc.*

enum EvenOddFillRuleState:
	case Outside
	case Inside
	/** BorderN means the north side is inside the shape and the south side is outside */
	case BorderN
	case BorderS

enum PipeSection:
	case Null, NS, EW, NE, NW, SE, SW

	def toChar:Char =
		import PipeSection.*
		this match
			case Null => '.'
			case NS => '|'
			case EW => '-'
			case NE => 'L'
			case NW => 'J'
			case SW => '7'
			case SE => 'F'

	def connectsIn(d:Direction): Boolean =
		import Direction.*
		this match
			case Null => false
			case NS => d == Up || d == Down
			case NE => d == Up || d == Right
			case NW => d == Up || d == Left
			case SE => d == Down || d == Right
			case SW => d == Down || d == Left
			case EW => d == Right || d == Left

	def arbitraryConnection:Direction =
		Direction.values.find(this.connectsIn).get

	def exiting(entering: Direction): Direction =
		import Direction.*
		entering match
			case Up =>
				this match
					case NS => Up
					case SE => Right
					case SW => Left
					case x => throw new MatchError(x)
			case Down =>
				this match
					case NS => Down
					case NE => Right
					case NW => Left
					case x => throw new MatchError(x)
			case Right =>
				this match
					case EW => Right
					case NW => Up
					case SW => Down
					case x => throw new MatchError(x)
			case Left =>
				this match
					case EW => Left
					case NE => Up
					case SE => Down
					case x => throw new MatchError(x)

/** represents being in the pipe section at `(x,y)` and facing in the `direction` direction */
case class Position(x:Int, y:Int, direction:Direction):
	def next(maze:Seq[Seq[PipeSection]]): Position =
		import Direction.*
		val Vector(dx, dy) = direction.toUnitVector

		val nextX = this.x + dx
		val nextY = this.y + dy

		val nextPipePiece = maze(nextY)(nextX)

		val nextDirection = nextPipePiece.exiting(this.direction)

		Position(nextX, nextY, nextDirection)

object PipeSection:
	def fromChar(c:Char): PipeSection =
		c match
			case '.' => Null
			case '|' => NS
			case '-' => EW
			case 'L' => NE
			case 'J' => NW
			case '7' => SW
			case 'F' => SE

def pipeLength(start: Position, maze: Seq[Seq[PipeSection]]): Int =
	@tailrec def rec(current: Position, count:Int):Int =
		if current == start then
			count
		else
			rec(current.next(maze), count + 1)

	rec(start.next(maze), 1)

/** where noise means those pipe sections that are not connected to the main loop */
def mazeWithoutNoise(start: Position, maze: Seq[Seq[PipeSection]]): Seq[Seq[PipeSection]] =
	/** Returns a grid, where each element indicates whether the cell is a part of the main pipe */
	def isPartOfPipe: Seq[Seq[Boolean]] =
		import scala.collection.mutable
		val partOfPipe:Seq[mutable.Seq[Boolean]] =
			maze.map(line => mutable.Seq.fill(line.length)(false))

		partOfPipe(start.y)(start.x) = true
		var position: Position = start.next(maze)
		while position != start do
			partOfPipe(position.y)(position.x) = true
			position = position.next(maze)

		partOfPipe.map(_.to(Seq))

	maze.zip(isPartOfPipe)
		.map: (inputRow, partOfPipeRow) =>
			inputRow.zip(partOfPipeRow)
				.map: (inputElem, partOfPipeElem) =>
					if partOfPipeElem then inputElem else PipeSection.Null

def pipeArea(mazeWithoutNoise: Seq[Seq[PipeSection]]): Int =
	mazeWithoutNoise
		.map: row =>
			row
				.foldLeft((0, EvenOddFillRuleState.Outside)): (folding, pipeSection) =>
					val (insideCount, fillRuleState) = folding
					import PipeSection.*
					import EvenOddFillRuleState.*
					pipeSection match
					case Null =>
						(insideCount + (if fillRuleState == Inside then 1 else 0), fillRuleState)
					case EW =>
						(insideCount, fillRuleState)
					case NS =>
						(insideCount, (if fillRuleState == Inside then Outside else Inside))
					case NE =>
						(insideCount, (fillRuleState match
							case Outside => BorderN
							case Inside => BorderS
							case x => throw new MatchError(x)
						))
					case SE =>
						(insideCount, (fillRuleState match
							case Outside => BorderS
							case Inside => BorderN
							case x => throw new MatchError(x)
						))
					case NW =>
						(insideCount, (fillRuleState match
							case BorderN => Outside
							case BorderS => Inside
							case x => throw new MatchError(x)
						))
					case SW =>
						(insideCount, (fillRuleState match
							case BorderN => Inside
							case BorderS => Outside
							case x => throw new MatchError(x)
						))
				._1
		.sum

object Day10:
	def main(args:Array[String]):Unit =
		import java.nio.file.*
		val inputString = os.read.lines(os.pwd / "input.txt")
		val (start, startSection) = {
			val startY = inputString.indexWhere(_.contains('S'))
			val startX = inputString(startY).indexOf('S')

			val northConnects = PipeSection.fromChar(inputString(startY - 1)(startX)).connectsIn(Direction.Down)
			val southConnects = PipeSection.fromChar(inputString(startY + 1)(startX)).connectsIn(Direction.Up)
			val westConnects = PipeSection.fromChar(inputString(startY)(startX - 1)).connectsIn(Direction.Right)
			val eastConnects = PipeSection.fromChar(inputString(startY)(startX + 1)).connectsIn(Direction.Left)

			val startSection = (northConnects, southConnects, westConnects, eastConnects) match
				case (true, true, false, false) => PipeSection.NS
				case (true, false, true, false) => PipeSection.NW
				case (true, false, false, true) => PipeSection.NE
				case (false, true, true, false) => PipeSection.SW
				case (false, true, false, true) => PipeSection.SE
				case (false, false, true, true) => PipeSection.EW
				case _ => ???

			(Position(startX, startY, startSection.arbitraryConnection), startSection)
		}
		val input = inputString.map:
			_.map: char =>
				if 'S' == char then
					startSection
				else
					PipeSection.fromChar(char)

		System.out.println(s"part 1: ${pipeLength(start, input) / 2}")
		System.out.println(s"part 2: ${pipeArea(mazeWithoutNoise(start, input))}")
