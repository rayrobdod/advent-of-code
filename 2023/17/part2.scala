//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2
//> using file ../Grid.scala

import scala.collection.mutable
import name.rayrobdod.aoc.*

case class PathfindState(cost: Int, previousMove: Direction, previousMoveRepeat: Int):
	def toSeen: PathfindSeen =
		PathfindSeen(
			this.previousMove,
			this.previousMoveRepeat,
		)

	def allowedNextMoves(minimumStraight: Int, maximumStraight: Int): Set[Direction] =
		if 0 == previousMoveRepeat then
			Direction.values.toSet
		else if previousMoveRepeat < minimumStraight then
			Set(previousMove)
		else if previousMoveRepeat >= maximumStraight then
			previousMove.orthogonal
		else
			previousMove.orthogonal + previousMove


val PathfindOrdering:Ordering[(Point, PathfindState)] =
	Ordering
		.by[(Point, PathfindState), Int](- _._2.cost)
		.orElseBy({(position, _) => position.x + position.y})

case class PathfindSeen(previousMove: Direction, previousMoveRepeat: Int)

def pathfind(grid: Grid[Int], minimumStraight: Int, maximumStraight: Int): Int =
	val initial = Point(0, 0)
	val target = Point(grid.width - 1, grid.height - 1)

	val resultSeen = grid.explore(
		((initial, new PathfindState(0, null, 0))),
		PathfindOrdering,
		_.toSeen,
		(p, _) => p != target,
		(_, s) => s.allowedNextMoves(minimumStraight, maximumStraight),
		{(currentPosition, currentState, nextPosition, nextMove, nextDeltaCost) =>
			val nextCost = currentState.cost + nextDeltaCost
			val nextDirectionRepeat =
				if nextMove == currentState.previousMove then
					1 + currentState.previousMoveRepeat
				else
					1
			new PathfindState(nextCost, nextMove, nextDirectionRepeat)
		},
	)

	val result = resultSeen._1

	result.get._2.cost
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
