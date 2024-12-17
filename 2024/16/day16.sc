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
