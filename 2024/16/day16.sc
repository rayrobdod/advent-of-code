//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using file ../../2023/Grid.scala

val inputFile = os.pwd / "input.txt"
val turnScore = 1000
val moveScore = 1

import scala.collection.mutable
import name.rayrobdod.aoc.*

val (start: Point, end: Point, maze: Grid[Boolean]) =
	val charGrid = Grid.fromStrings(os.read.lines(inputFile))
	val start = charGrid.indexOf('S')
	val end = charGrid.indexOf('E')
	val maze = charGrid.map:
		case '#' => false
		case _ => true
	(start, end, maze)

val part1Exploration = maze.explore[(Direction, Int), Unit](
	start = (start, (Direction.Right, 0)),
	priority = {
		scala.math.Ordering.by[(Point, (Direction, Int)), Int](x =>
			val (point, (direction, score)) = x
			-(score)
		)
	},
	toSeen = _ => (),
	continue = (p, _) => p != end,
	allowedNextMoves =
		case (p, (prevDirection, _)) =>
			Direction.values.toSet
				.-(-prevDirection)
				.filter: d =>
					maze(p + d.toUnitVector)
	,
	updatedState = (_, prevState, _, newDirection, _) =>
		val (prevDirection, prevScore) = prevState
		if prevDirection == newDirection then
			(newDirection, prevScore + moveScore)
		else
			(newDirection, prevScore + moveScore + turnScore)
)
val part1 = part1Exploration._1.get._2._2

println(s"Checked spaces: ${part1Exploration._2.size}")
println(s"part 1: ${part1}")



final case class Position(p: Point, d: Direction)
final case class QueuedState(current: Position, score: Int, previous: Set[Position])

given scala.math.Ordering[QueuedState] =
	scala.math.Ordering.by[QueuedState, Int](- _.score)

val part2Exploration =
	import scala.collection.mutable
	val toVisit = mutable.PriorityQueue.empty[QueuedState]
	toVisit.enqueue(QueuedState(Position(start, Direction.Right), 0, Set.empty))
	val seen = mutable.Map.empty[Position, Set[Position]]

	while toVisit.nonEmpty && toVisit.head.score <= part1  do
		val QueuedState(currentPosition, currentScore, previousPositions) = toVisit.dequeue()
		val Position(currentPoint, currentFacing) = currentPosition

		seen += ((currentPosition, previousPositions))

		val allowedNextPositions =
			val forwardPoint = currentPoint + currentFacing.toUnitVector
			val forward = (forwardPoint, currentFacing, currentScore + moveScore)
			val turnRight = (currentPoint, currentFacing.turnRight, currentScore + turnScore)
			val turnLeft = (currentPoint, currentFacing.turnLeft, currentScore + turnScore)

			if maze(forwardPoint) then
				Seq(forward, turnRight, turnLeft)
			else
				Seq(turnRight, turnLeft)
			end if
		end allowedNextPositions

		for nextMove <- allowedNextPositions do
			val (nextPoint, nextFacing, nextScore) = nextMove
			val nextPosition = Position(nextPoint, nextFacing)

			if ! seen.keySet.contains(nextPosition) then
				def matchesNext(queued: QueuedState): Boolean =
					val queuedPosition = queued.current
					queuedPosition == nextPosition

				if ! toVisit.exists(matchesNext) then
					toVisit.enqueue(QueuedState(nextPosition, nextScore, Set(currentPosition)))
				else
					toVisit.mapInPlace: queued =>
						if matchesNext(queued) then
							if nextScore == queued.score then
								QueuedState(nextPosition, nextScore, queued.previous + currentPosition)
							else if nextScore < queued.score then
								QueuedState(nextPosition, nextScore, Set(currentPosition))
							else
								queued
							end if
						else
							queued
						end if
				end if
			end if
		end for
	end while

	seen.toMap
end part2Exploration

val part2 =
	val endPositions = part2Exploration.keySet.filter(_.p == end)

	val toVisit = mutable.Queue.from[Position](endPositions)
	val seen = mutable.Set.empty[Position]

	while toVisit.nonEmpty do
		val current = toVisit.dequeue()
		seen += current

		part2Exploration(current)
			.foreach: p =>
				toVisit.enqueue(p)
	end while

	seen.toSet.map(_._1)
end part2

/*
println:
	Grid.tabulate(maze.width, maze.height): p =>
		if part2.contains(p) then
			'O'
		else if maze(p) then
			'.'
		else
			'#'
	.mkString
*/

println(s"part 2: ${part2.size}")
