//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

enum Direction:
	case North, East, West, South

	def movement: (Int, Int) =
		this match
			case North => (0, -1)
			case South => (0, 1)
			case East => (1, 0)
			case West => (-1, 0)

enum PipeSection:
	case Null, NS, EW, NE, NW, SE, SW

	def connectsIn(d:Direction): Boolean =
		import Direction.*
		this match
			case Null => false
			case NS => d == North || d == South
			case NE => d == North || d == East
			case NW => d == North || d == West
			case SE => d == South || d == East
			case SW => d == South || d == West
			case EW => d == East || d == West

	def arbitraryConnection:Direction =
		Direction.values.find(this.connectsIn).get

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
		val (dx, dy) = direction.movement

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

object Day10Part1:
	def main(args:Array[String]):Unit =
		import java.nio.file.*
		val inputString = os.read.lines(os.pwd / "input.txt")
		val (start, startSection) = {
			val startY = inputString.indexWhere(_.contains('S'))
			val startX = inputString(startY).indexOf('S')

			val northConnects = PipeSection.fromChar(inputString(startY - 1)(startX)).connectsIn(Direction.South)
			val southConnects = PipeSection.fromChar(inputString(startY + 1)(startX)).connectsIn(Direction.North)
			val westConnects = PipeSection.fromChar(inputString(startY)(startX - 1)).connectsIn(Direction.East)
			val eastConnects = PipeSection.fromChar(inputString(startY)(startX + 1)).connectsIn(Direction.West)

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

		var length: Int = 1
		var position: Position = start.next(input)
		while position != start do
			length += 1
			position = position.next(input)

		System.out.println(length / 2)
