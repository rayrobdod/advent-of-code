//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2


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

	def toUnitVector: Vector =
		this match
			case Up => Vector(0, -1)
			case Down => Vector(0, 1)
			case Left => Vector(-1, 0)
			case Right => Vector(1, 0)

	def turns: Set[Direction] =
		this match
			case Up | Down => Set(Left, Right)
			case Left | Right => Set(Up, Down)
end Direction

object Direction:
	def all: Set[Direction] =
		Set(Up, Left, Right, Down)
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

case class PathfindState(cost: Int, position: Point, previousMove: Direction, previousMoveRepeat: Int):
	def toSeen: PathfindSeen =
		PathfindSeen(
			this.position,
			this.previousMove,
			this.previousMoveRepeat,
		)

	def allowedNextMoves(minimumStraight: Int, maximumStraight: Int): Set[Direction] =
		if 0 == previousMoveRepeat then
			Direction.all
		else if previousMoveRepeat < minimumStraight then
			Set(previousMove)
		else if previousMoveRepeat >= maximumStraight then
			previousMove.turns
		else
			previousMove.turns + previousMove


object PathfindState:
	given Ordering[PathfindState] =
		Ordering
			.by[PathfindState, Int](- _.cost)
			.orElseBy(s => s.position.x + s.position.y)

case class PathfindSeen(position: Point, previousMove: Direction, previousMoveRepeat: Int)

def pathfind(grid: Grid[Int], minimumStraight: Int, maximumStraight: Int): Int =
	val initial = Point(0, 0)
	val target = Point(grid.width - 1, grid.height - 1)

	val toVisit = mutable.PriorityQueue.empty[PathfindState]
	toVisit.enqueue(new PathfindState(0, initial, null, 0))
	val seen = mutable.Set.empty[PathfindSeen]

	while toVisit.head.position != target do
		val current = toVisit.dequeue()

		seen += current.toSeen

		val allowedMoves = current.allowedNextMoves(minimumStraight, maximumStraight)

		for nextMove <- allowedMoves do
			val nextPosition = current.position + nextMove.toUnitVector
			if grid.isDefinedAt(nextPosition) then
				val nextDeltaCost = grid(nextPosition)
				val nextCost = current.cost + nextDeltaCost
				val nextDirectionRepeat =
					if nextMove == current.previousMove then
						1 + current.previousMoveRepeat
					else
						1

				val nextState = new PathfindState(nextCost, nextPosition, nextMove, nextDirectionRepeat)
				if ! seen.contains(nextState.toSeen) then
					if ! toVisit.exists(_.toSeen == nextState.toSeen) then
						toVisit.enqueue(nextState)
					else
						toVisit.mapInPlace: previous =>
							if previous.toSeen == nextState.toSeen then
								if previous.cost < nextCost then
									previous
								else
									nextState
							else
								previous
				end if
			end if
		end for
	end while

	toVisit.dequeue.cost
end pathfind

object Day17:
	def main(args:Array[String]):Unit =
		val input: Grid[Int] =
			Grid:
				os.read.lines(os.pwd / "input.txt")
					.map: line =>
						line.map: c =>
							(c - '0').intValue

		System.out.print("part 1: ")
		System.out.println:
			pathfind(input, 1, 3)

		System.out.print("part 2: ")
		System.out.println:
			pathfind(input, 4, 10)
