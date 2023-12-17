package name.rayrobdod.aoc

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

	def orthogonal: Set[Direction] =
		this match
			case Up | Down => Set(Left, Right)
			case Left | Right => Set(Up, Down)

	def show: Char =
		this match
			case Up => '^'
			case Down => 'V'
			case Left => '>'
			case Right => '<'
end Direction

class Grid[A](private val backing: Seq[Seq[A]]):
	def width: Int = backing(0).size
	def height: Int = backing.size

	def apply(p: Point): A = backing(p.y)(p.x)

	def isDefinedAt(p: Point): Boolean =
		0 <= p.x && p.x < width && 0 <= p.y && p.y < height

	def updated(p: Point, newValue: A): Grid[A] =
		Grid:
			backing.updated(p.y, backing(p.y).updated(p.x, newValue))

	def map[B](fn: A => B): Grid[B] =
		Grid:
			backing.map: row =>
				row.map:
					fn

	def zip[B](other: Grid[B]): Grid[(A, B)] =
		Grid:
			this.backing.zip(other.backing)
				.map: (thisRow, otherRow) =>
					thisRow.zip(otherRow)

	def mkString: String =
		backing.map(_.mkString).mkString("\n")

	def mkString(sep: String): String =
		backing.map(_.mkString(sep)).mkString("\n")

	def explore[QueuedState, SeenState](
		start: (Point, QueuedState),
		priority: scala.math.Ordering[(Point, QueuedState)],
		toSeen: QueuedState => SeenState,
		continue: (Point, QueuedState) => Boolean,
		allowedNextMoves: (Point, QueuedState) => Set[Direction],
		updatedState: (Point, QueuedState, Point, Direction, A) => QueuedState,
	):(Option[(Point, QueuedState)], Set[(Point, SeenState)]) =
		import scala.collection.mutable
		val toVisit = mutable.PriorityQueue.empty[(Point, QueuedState)](using priority)
		toVisit.enqueue(start)
		val seen = mutable.Set.empty[(Point, SeenState)]

		while toVisit.nonEmpty && continue.tupled(toVisit.head) do
			val (currentPosition, currentState) = toVisit.dequeue()

			//System.out.print(s"ENTERING: $currentPosition $currentState")
			//new java.util.Scanner(System.in).nextLine()

			val currentSeen = toSeen(currentState)
			seen += ((currentPosition, currentSeen))

			for nextMove <- allowedNextMoves(currentPosition, currentState) do
				val nextPosition = currentPosition + nextMove.toUnitVector
				if this.isDefinedAt(nextPosition) then
					val nextState = updatedState(currentPosition, currentState, nextPosition, nextMove, this(nextPosition))
					val nextSeen = toSeen(nextState)
					if ! seen.contains((nextPosition, nextSeen)) then
						def matchesNext(queued: (Point, QueuedState)): Boolean =
							val (queuedPosition, queuedState) = queued
							queuedPosition == nextPosition && toSeen(queuedState) == nextSeen

						if ! toVisit.exists(matchesNext) then
							toVisit.enqueue((nextPosition, nextState))
						else
							toVisit.mapInPlace: queued =>
								if matchesNext(queued) then
									if priority.compare(queued, ((nextPosition, nextState))) >= 0 then
										queued
									else
										(nextPosition, nextState)
									end if
								else
									queued
								end if
						end if
					end if
				end if
			end for
		end while

		return (toVisit.headOption, seen.toSet)
	end explore
end Grid

object Grid:
	def tabulate[A](width: Int, height: Int)(fn: Point => A): Grid[A] =
		Grid:
			Seq.tabulate(height, width): (y, x) =>
				fn(Point(x, y))
	end tabulate
end Grid
