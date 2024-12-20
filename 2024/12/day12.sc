//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using file ../../2023/Grid.scala

import scala.collection.mutable
import name.rayrobdod.aoc.*

val input: Grid[Char] =
	Grid.fromStrings(os.read.lines(os.pwd / "input.txt"))

val regions: Seq[Set[Point]] = locally:
	val builder = Seq.newBuilder[Set[Point]]
	val seen: mutable.Set[Point] = mutable.Set.empty
	input.indices
		.foreach: p =>
			if ! seen.contains(p) then
				val cropType = input(p)
				// flood-fill
				val region = input.explore[Unit, Unit, Unit](
					start = (p, ()),
					priority = scala.math.Ordering.fromLessThan((_, _) => true),
					toSeen = u => (u, u),
					continue = (_, _) => true,
					allowedNextMoves = (p, _) =>
						Direction.values
							.filter: d =>
								input.isDefinedAt(p + d.toUnitVector) &&
									cropType == input(p + d.toUnitVector)
							.toSet
						,
					updatedState = (_, _, _, _, _) => (),
				)._2.keySet.map(_._1)
				seen ++= region
				builder += region
			end if
	builder.result

val part1 = regions
	.view
	.map: region =>
		val area = region.size
		val perimeter = region
			.view
			.map: p =>
				Direction.values
					.count: d =>
						(! input.isDefinedAt(p + d.toUnitVector)) ||
							input(p) != input(p + d.toUnitVector)
			.sum

		area * perimeter
	.sum

println(s"part 1: ${part1}")

val part2 = regions
	.view
	.map: region =>
		val area = region.size
		val sides = region
			.view
			.map: p =>
				Direction.values
					.count: d =>
						val selfCrop = Option(input(p))
						val acrossFenceCrop = input.get(p + d.toUnitVector)

						if selfCrop == acrossFenceCrop then
							// no fence here
							false
						else
							val adjacentCrop = input.get(p + d.turnRight.toUnitVector)
							val acrossAdjacentCrop = input.get(p + d.toUnitVector + d.turnRight.toUnitVector)

							if adjacentCrop != selfCrop then
								// corner towards self
								true
							else if adjacentCrop == acrossAdjacentCrop then
								// corner away from self
								true
							else
								// fence does not turn here
								false
							end if
						end if
			.sum

		area * sides
	.sum

println(s"part 2: ${part2}")
