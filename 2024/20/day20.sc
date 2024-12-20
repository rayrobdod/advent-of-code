//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using file ../../2023/Grid.scala

val inputFile = os.pwd / "input.txt"

import scala.collection.immutable.SortedMap
import name.rayrobdod.aoc.*

val (start: Point, end: Point, maze: Grid[Boolean]) =
	val charGrid = Grid.fromStrings(os.read.lines(inputFile))
	val start = charGrid.indexOf('S')
	val end = charGrid.indexOf('E')
	val maze = charGrid.map:
		case '#' => false
		case _ => true
	(start, end, maze)

val path = maze
	.explore[Int, Unit, Int](
		start = (start, 0),
		priority = scala.math.Ordering.by[(Point, Int), Int](- _._2),
		toSeen = score => ((), score),
		continue = (p, _) => true,
		allowedNextMoves =
			case (p, _) =>
				Direction.values.toSet
					.filter: d =>
						val nextSpace = p + d.toUnitVector
						maze.isDefinedAt(nextSpace) && maze(nextSpace)
		,
		updatedState = (_, prevState, _, _, _) => prevState + 1,
	)
	._2
	.map:
		case ((point, ()), score) => (point, score)

locally:
	val cheatPossibilities = Seq(
		Vector(0,2), Vector(1,1), Vector(2,0), Vector(1,-1),
		Vector(0,-2), Vector(-1,-1), Vector(-2,0), Vector(-1,1),
	)

	val cheats =
		for
			cheatStart <- maze.indices
			if maze(cheatStart);
			cheatPath <- cheatPossibilities;
			cheatEnd = cheatStart + cheatPath
			if maze.isDefinedAt(cheatEnd)
			if maze(cheatEnd)
			cheatSaving = path(cheatEnd) - path(cheatStart) - 2
			if cheatSaving > 0
		yield
			cheatSaving

	//println(cheats.groupMapReduce(k => k)(_ => 1)(_+_).to(SortedMap))

	println(s"part 1: ${cheats.count(_ >= 100)}")

locally:
	val cheatPossibilities =
		for
			x <- -20 to 20;
			y <- -(20 - x.abs) to (20 - x.abs)
		yield
			Vector(x, y)

	val cheats =
		for
			cheatStart <- maze.indices
			if maze(cheatStart);
			cheatPath <- cheatPossibilities;
			cheatEnd = cheatStart + cheatPath
			if maze.isDefinedAt(cheatEnd)
			if maze(cheatEnd)
			cheatSaving = path(cheatEnd) - path(cheatStart) - cheatPath.x.abs - cheatPath.y.abs
			if cheatSaving > 50
		yield
			cheatSaving

	//println(cheats.groupMapReduce(k => k)(_ => 1)(_+_).to(SortedMap))

	println(s"part 2: ${cheats.count(_ >= 100)}")
