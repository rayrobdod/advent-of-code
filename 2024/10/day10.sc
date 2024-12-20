//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using file ../../2023/Grid.scala

import name.rayrobdod.aoc.*

val input: Grid[Int] =
	Grid.fromStrings(os.read.lines(os.pwd / "input.txt"))
		.map(_ - '0')

val part1 = input
	.indicesWhere(0 == _)
	.map: startPoint =>
		input.explore[Unit, Unit, Unit](
			start = (startPoint, ()),
			priority = scala.math.Ordering.fromLessThan((_, _) => true),
			toSeen = u => (u, u),
			continue = (_, _) => true,
			allowedNextMoves = (currentPos, _) =>
				val currentHeight = input(currentPos)
				Direction.values
					.filter: d =>
						val nextPos = currentPos + d.toUnitVector
						input.isDefinedAt(nextPos) && {
							val nextHeight = input(nextPos)
							currentHeight + 1 == nextHeight
						}
					.to(Set)
				,
			updatedState = (_, _, _, _, _) => (),
		)
		._2
		.keySet
		.count: (p, _) =>
			input(p) == 9
	.sum

println(s"part 1: ${part1}")

val part2 = input
	.indicesWhere(0 == _)
	.map: startPoint =>
		input.explore[List[Direction], List[Direction], Unit](
			start = (startPoint, Nil),
			priority = scala.math.Ordering.fromLessThan((_, _) => true),
			toSeen = u => (u, ()),
			continue = (_, _) => true,
			allowedNextMoves = (currentPos, _) =>
				val currentHeight = input(currentPos)
				Direction.values
					.filter: d =>
						val nextPos = currentPos + d.toUnitVector
						input.isDefinedAt(nextPos) && {
							val nextHeight = input(nextPos)
							currentHeight + 1 == nextHeight
						}
					.to(Set)
				,
			updatedState = (_, pathSoFar, _, next, _) => next :: pathSoFar,
		)
		._2
		.keySet
		.count: (p, _) =>
			input(p) == 9
	.sum

println(s"part 2: ${part2}")
