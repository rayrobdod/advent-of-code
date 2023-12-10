//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.collection.mutable

enum EvenOddFillRuleState:
	case Outside, Inside
	// BorderN the north half is inside and the south side is outside
	case BorderN, BorderS

enum Direction:
	case North, East, West, South

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

	def exiting(entering: Direction): Direction =
		import Direction.*
		entering match
			case North =>
				this match
					case NS => North
					case SE => East
					case SW => West
			case South =>
				this match
					case NS => South
					case NE => East
					case NW => West
			case East =>
				this match
					case EW => East
					case NW => North
					case SW => South
			case West =>
				this match
					case EW => West
					case NE => North
					case SE => South

/** represents being in the pipe section at `(x,y)` and facing in the `direction` direction */
case class Position(x:Int, y:Int, direction:Direction):
	def next(maze:Seq[Seq[PipeSection]]): Position =
		import Direction.*
		val (dx, dy) = direction match
			case North => (0, -1)
			case South => (0, 1)
			case East => (1, 0)
			case West => (-1, 0)

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
			// hardcoded
			case 'S' => NE

object Day10Part2:
	def main(args:Array[String]):Unit =
		import java.nio.file.*
		val inputString = os.read.lines(os.pwd / "input.txt")
		val start = {
			val startY = inputString.indexWhere(_.contains('S'))
			val startX = inputString(startY).indexOf('S')
			Position(startX, startY, Direction.North)
		}
		val input = inputString.map(_.map(PipeSection.fromChar))

		val partOfPipe:Seq[mutable.Seq[Boolean]] =
			input.map(line => mutable.Seq.fill(line.length)(false))

		partOfPipe(start.y)(start.x) = true
		var position: Position = start.next(input)
		while position != start do
			partOfPipe(position.y)(position.x) = true
			position = position.next(input)

		val inputWithoutNoise: Seq[Seq[PipeSection]] = input.zip(partOfPipe)
			.map: (inputRow, partOfPipeRow) =>
				inputRow.zip(partOfPipeRow)
					.map: (inputElem, partOfPipeElem) =>
						if partOfPipeElem then inputElem else PipeSection.Null

		val count = inputWithoutNoise
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
							))
						case SE =>
							(insideCount, (fillRuleState match
								case Outside => BorderS
								case Inside => BorderN
							))
						case NW =>
							(insideCount, (fillRuleState match
								case BorderN => Outside
								case BorderS => Inside
							))
						case SW =>
							(insideCount, (fillRuleState match
								case BorderN => Inside
								case BorderS => Outside
							))
					._1
			.sum

		System.out.println(count)
