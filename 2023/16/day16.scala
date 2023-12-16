//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.collection.mutable

enum OneOrTwo[A]:
	case One(_1: A)
	case Two(_1: A, _2: A)

	def map[B](fn: A => B): OneOrTwo[B] =
		this match
			case One(_1) =>
				One(fn(_1))
			case Two(_1, _2) =>
				Two(fn(_1), fn(_2))

	def foreach(fn: A => Unit): Unit =
		this match
			case One(_1) =>
				fn(_1)
			case Two(_1, _2) =>
				fn(_1)
				fn(_2)
end OneOrTwo

enum Part:
	case Empty
	case Mirror, BackMirror
	case NSSplitter, EWSplitter

	def directionChange(in: Direction): OneOrTwo[Direction] =
		import Direction.*
		this match
			case Empty =>
				OneOrTwo.One(in)
			case Mirror =>
				OneOrTwo.One:
					in match
						case Up => Right
						case Left => Down
						case Down => Left
						case Right => Up
			case BackMirror =>
				OneOrTwo.One:
					in match
						case Up => Left
						case Left => Up
						case Down => Right
						case Right => Down
			case NSSplitter =>
				in match
					case Up | Down => OneOrTwo.One(in)
					case Left | Right => OneOrTwo.Two(Up, Down)
			case EWSplitter =>
				in match
					case Up | Down => OneOrTwo.Two(Left, Right)
					case Left | Right => OneOrTwo.One(in)
	end directionChange
end Part

object Part:
	def fromChar(c: Char): Part =
		c match
			case '.' => Empty
			case '/' => Mirror
			case '\\' => BackMirror
			case '|' => NSSplitter
			case '-' => EWSplitter
end Part

enum Direction:
	case Up, Down, Left, Right

	def move(p: Point): Point =
		this match
			case Up => Point(p.x, p.y - 1)
			case Down => Point(p.x, p.y + 1)
			case Left => Point(p.x - 1, p.y)
			case Right => Point(p.x + 1, p.y)
end Direction

case class Point(x: Int, y: Int)

class Grid[A](backing: Seq[Seq[A]]):
	def apply(p: Point) = backing(p.y)(p.x)

	def isDefinedAt(p: Point): Boolean =
		0 <= p.x && p.x < width && 0 <= p.y && p.y < height

	def width = backing(0).length
	def height = backing.length
end Grid

case class PositionAndDirection(point: Point, direction: Direction)

def energized(initial: PositionAndDirection, contraption: Grid[Part]): Int =
	val toVisit = mutable.Queue.empty[PositionAndDirection]
	toVisit.enqueue(initial)
	val seen = mutable.Set.empty[PositionAndDirection]

	while toVisit.nonEmpty do
		val current = toVisit.dequeue()
		seen += current

		val nextPosition = current.direction.move(current.point)
		if contraption.isDefinedAt(nextPosition) then
			val nextDirections = contraption(nextPosition).directionChange(current.direction)
			val nextStances = nextDirections.map: nextDirection =>
				PositionAndDirection(nextPosition, nextDirection)
			nextStances.foreach: nextStance =>
				if ! seen.contains(nextStance) then
					toVisit.enqueue(nextStance)
	end while

	seen
		.to(Set)
		.map(_.point)
		// still a set, so is still distinct
		.size
		// `seen` will include 'initial', which is off the grid
		.-(1)
end energized

def possibleStarts(contraption: Grid[_]): Seq[PositionAndDirection] =
	val xs = (0 until contraption.width).flatMap: x =>
		Seq(
			PositionAndDirection(Point(x, -1), Direction.Down),
			PositionAndDirection(Point(x, contraption.height), Direction.Up)
		)

	val ys = (0 until contraption.height).flatMap: y =>
		Seq(
			PositionAndDirection(Point(-1, y), Direction.Right),
			PositionAndDirection(Point(contraption.width, y), Direction.Left)
		)

	xs ++ ys
end possibleStarts

object Day16:
	def main(args:Array[String]):Unit =
		val input = Grid:
			os.read.lines(os.pwd / "input.txt")
				.map: line =>
					line.map:
						Part.fromChar

		System.out.print("part 1: ")
		System.out.println:
			val initial = PositionAndDirection(Point(-1, 0), Direction.Right)
			energized(initial, input)

		System.out.print("part 2: ")
		System.out.println:
			possibleStarts(input)
				.map: start =>
					energized(start, input)
				.max
