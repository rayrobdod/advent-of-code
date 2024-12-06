//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using file ../../2023/Grid.scala

import name.rayrobdod.aoc.*

val input_raw: Grid[Char] = Grid.fromStrings(os.read.lines(os.pwd / "input.txt"))
val startPosition = input_raw.indexOf('^')
val startDirection = Direction.Up
val input: Grid[Boolean] = input_raw.map:
	case '#' => false
	case _ => true

extension (self: Grid[Boolean])
	def exploreAsGuard: (Option[(Point, Direction)], Set[(Point, Direction)]) =
		self.explore[Direction, Direction](
			start = (startPosition, startDirection),
			priority = scala.math.Ordering.fromLessThan({(_, _) => true}),
			toSeen = d => d,
			continue = {(p, d) => self.isDefinedAt(p + d.toUnitVector)},
			allowedNextMoves = {(p, d) =>
				if self.getOrElse(p + d.toUnitVector, true) then
					Set(d)
				else
					val d1 = d.turnRight
					if self.getOrElse(p + d1.toUnitVector, true) then
						Set(d1)
					else
						val d2 = d1.turnRight
						if self.getOrElse(p + d2.toUnitVector, true) then
							Set(d2)
						else
							throw new Exception("Expect guard to be able to leave the way guard entered space")
						end if
					end if
				end if
			},
			updatedState = (_, _, _, d, _) => d,
		)

val inputExploration = input.exploreAsGuard

val part1 = inputExploration._2.map(_._1).size + 1
System.out.println(s"part 1: ${part1}")


val unmodifiedGuardPath = inputExploration._2.map(_._1) + inputExploration._1.get._1

// if the new obstruction is not in the guard's original path,
// then the new obstruction will not change the guards path and cannot cause the guard's path to change to a loop
// so should not waste time evaluating what happens when placing an obstruction outside the guard's original path
val modifiedInputs = input.indices.collect:
	case p if unmodifiedGuardPath(p) && p != startPosition =>
		input.updated(p, false)

val part2 = modifiedInputs.count: modifiedInput =>
	val exploration = modifiedInput.exploreAsGuard

	// in this case, defined means walking off the edge of the map,
	// while empty means found a loop
	val isLoop = exploration._1.isEmpty

	if false then
		println(
			Grid
				.tabulate(input.width, input.height): p =>
					if ! modifiedInput(p) then
						'#'
					else if exploration._2.map(_._1).contains(p) then
						'O'
					else
						' '
				.mkString
		)
		println()
	end if
	isLoop


System.out.println(s"part 2: ${part2}")
