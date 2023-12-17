//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2


// doesn't find the optimal path because it cannot backtrack
// and unlike normal distrika, the path that reaches a spot with the lowest cost
// is not necessarily the path that passes through that spot with the lowest cost



import scala.collection.mutable

case class Vector(x: Int, y: Int):
	def unary_- : Vector = Vector(-x, -y)
	def +(v: Vector): Vector = Vector(this.x + v.x, this.y + v.y)
	def +(v: Point): Point = Point(this.x + v.x, this.y + v.y)
	def -(v: Vector): Vector = Vector(this.x - v.x, this.y - v.y)
	def -(v: Point): Point = Point(this.x - v.x, this.y - v.y)
end Vector

case class Point(x: Int, y: Int):
	def +(v: Vector): Point = Point(this.x + v.x, this.y + v.y)
	def -(v: Vector): Point = Point(this.x - v.x, this.y - v.y)
end Point

enum Direction:
	case Up, Down, Left, Right

	def unary_- : Direction =
		this match
			case Up => Down
			case Down => Up
			case Left => Right
			case Right => Left

	def toVector: Vector =
		this match
			case Up => Vector(0, -1)
			case Down => Vector(0, 1)
			case Left => Vector(-1, 0)
			case Right => Vector(1, 0)

	def visualizeCharacter: Char =
		this match
			case Up => '^'
			case Down => 'V'
			case Left => '<'
			case Right => '>'

	def turns: Set[Direction] =
		this match
			case Up | Down => Set(Left, Right)
			case Left | Right => Set(Up, Down)
end Direction

class Grid[A](backing: Seq[Seq[A]]):
	def apply(p: Point): A = backing(p.y)(p.x)

	def isDefinedAt(p: Point): Boolean =
		0 <= p.x && p.x < width && 0 <= p.y && p.y < height

	def updated(p: Point, newValue: A): Grid[A] =
		Grid:
			backing.updated(p.y, backing(p.y).updated(p.x, newValue))

	def mkString: String =
		backing.map(_.mkString).mkString("\n")

	def mkString(sep: String): String =
		backing.map(_.mkString(sep)).mkString("\n")

	def width: Int = backing(0).size
	def height: Int = backing.size
end Grid

case class PathfindState(cost: Int, position: Point, previousMove: Direction)

object PathfindState:
	given Ordering[PathfindState] =
		Ordering
			.by[PathfindState, Int](- _.cost)
			.orElseBy(s => s.position.x + s.position.y)

def pathfind(grid: Grid[Int]): Int =
	val initial = Point(0, 0)
	val target = Point(grid.width - 1, grid.height - 1)

	val toVisit = mutable.PriorityQueue.empty[PathfindState]
	for (dir <- Seq(Direction.Right, Direction.Down)) do
		val moveTo = initial + dir.toVector
		toVisit.enqueue(new PathfindState(grid(moveTo), moveTo, dir))
	end for
	val seen = mutable.Map.empty[Point, Direction]

	while toVisit.head.position != target do
		System.out.println(visualize(toVisit, seen))

		val current = toVisit.dequeue()

		System.out.print(s"ENTERING: $current")
		new java.util.Scanner(System.in).nextLine()

		seen += ((current.position, current.previousMove))

		val isThreeLongStraightLine = locally:
			val position_1 = current.position
			val move_1 = current.previousMove
			val position_2 = position_1 - move_1.toVector
			val move_2 = seen.get(position_2)
			if Option(move_1) != move_2 then
				false
			else
				val position_3 = position_2 - move_1.toVector
				val move_3 = seen.get(position_3)
				if Option(move_1) != move_3 then
					false
				else
					true

		val allowedMoves =
			if isThreeLongStraightLine then
				current.previousMove.turns
			else
				current.previousMove.turns + current.previousMove

		for nextMove <- allowedMoves do
			val nextPosition = current.position + nextMove.toVector
			if grid.isDefinedAt(nextPosition) then
				val nextDeltaCost = grid(nextPosition)
				val nextCost = current.cost + nextDeltaCost

				if ! seen.contains(nextPosition) then
					if ! toVisit.exists(_.position == nextPosition) then
						toVisit.enqueue(new PathfindState(nextCost, nextPosition, nextMove))
					else
						toVisit.mapInPlace: previous =>
							if previous.position == nextPosition then
								if previous.cost < nextCost then
									previous
								else
									new PathfindState(nextCost, nextPosition, nextMove)
							else
								previous
				end if
			end if
		end for
	end while

	seen += ((toVisit.head.position, toVisit.head.previousMove))

	System.out.println(visualize(toVisit, seen))

	toVisit.dequeue.cost
end pathfind

def visualize(
	toVisit: mutable.PriorityQueue[PathfindState],
	seen: mutable.Map[Point, Direction]
):String =
	val size = 14
	val empty = Grid(Seq.fill(size, size)(' '))
	val _1 = toVisit.foldLeft(empty): (folding, value) =>
		val PathfindState(_, position, direction) = value
		val directionStr = (-direction).visualizeCharacter

		folding.updated(position, directionStr)

	val _2 = seen.foldLeft(_1): (folding, value) =>
		val (position, direction) = value
		val directionStr = (-direction).visualizeCharacter

		folding.updated(position, directionStr)
	_2.mkString


object Day17:
	def main(args:Array[String]):Unit =
		val input: Grid[Int] =
			Grid:
				os.read.lines(os.pwd / "sample.txt")
					.map: line =>
						line.map: c =>
							(c - '0').intValue

		System.out.print("part 1: ")
		System.out.println:
			pathfind(input)

		//System.out.print("part 2: ")
		//System.out.println:
