//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

enum Direction:
	case North, East, West, South

enum PipeSection:
	case Null, NS, EW, NE, NW, SE, SW

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

object Day10Part1:
	def main(args:Array[String]):Unit =
		import java.nio.file.*
		val inputString = os.read.lines(os.pwd / "input.txt")
		val start = {
			val startY = inputString.indexWhere(_.contains('S'))
			val startX = inputString(startY).indexOf('S')
			Position(startX, startY, Direction.North)
		}
		val input = inputString.map(_.map(PipeSection.fromChar))

		var length: Int = 1
		var position: Position = start.next(input)
		while position != start do
			length += 1
			position = position.next(input)

		System.out.println(length / 2)
