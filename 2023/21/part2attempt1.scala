//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2
//> using file ../Grid.scala

import scala.annotation.tailrec
import name.rayrobdod.aoc.*

def show(traversable: Grid[Boolean], positions: Set[Point]):String =
	positions
		.foldLeft(traversable.map(b => if b then '.' else '#')): (f, p) =>
			f.updated(p, 'O')
		.mkString

def diagonalPerimeter(radius: Int): Long =
	if radius == 0 then 1L else 4L * radius

def possibleSpacesAfterSteps(traversable: Grid[Boolean], start: Point, steps: Int): Int =
	def traversable2(p:Point):Boolean = traversable.isDefinedAt(p) && traversable(p)

	@tailrec def rec(currentPossiblePositions: Set[Point], stepsLeft:Int): Set[Point] =
		if 0 >= stepsLeft then
			currentPossiblePositions
		else
			val nextPositions = currentPossiblePositions
				.flatMap: p =>
					Direction.values
						.map: v =>
							p + v.toUnitVector
				.filter:
					traversable2
			rec(nextPositions, stepsLeft - 1)
	end rec

	rec(Set(start), steps).size
end possibleSpacesAfterSteps

/**
 * assumptions:
 *   * `traversable` is a square with an odd width
 *   * `start` is in center of `traversable`
 *   * traversable is true at all points in cardinal directions of start
 */
def possibleSpacesAfterSteps2(traversable: Grid[Boolean], start: Point, steps: Int): Long =
	val TABULATE_PARTS = false
	val maxX = traversable.width - 1
	val maxY = traversable.height - 1

	if steps <= traversable.width / 2 then
		// no parallel universes accessed
		possibleSpacesAfterSteps(traversable, start, steps)
	else
		val minorCornerSteps = (steps - traversable.width / 2 - 1) % traversable.width

		val minorCornerSpaces = Seq(
			possibleSpacesAfterSteps(traversable, Point(start.x, 0), minorCornerSteps),
			possibleSpacesAfterSteps(traversable, Point(start.x, maxY), minorCornerSteps),
			possibleSpacesAfterSteps(traversable, Point(0, start.y), minorCornerSteps),
			possibleSpacesAfterSteps(traversable, Point(maxX, start.y), minorCornerSteps),
		).sum

		if steps <= traversable.width then
			// original universe is not full
			if TABULATE_PARTS then System.out.print(f" (c) $minorCornerSteps%2d $minorCornerSpaces%3d")
			minorCornerSpaces + possibleSpacesAfterSteps(traversable, start, steps)
		else
			val majorCornerSteps = steps % traversable.width + traversable.width / 2

			val majorCornerSpaces = locally:
				if majorCornerSteps < traversable.width then
					0
				else
					Seq(
						possibleSpacesAfterSteps(traversable, Point(start.x, 0), majorCornerSteps),
						possibleSpacesAfterSteps(traversable, Point(start.x, maxY), majorCornerSteps),
						possibleSpacesAfterSteps(traversable, Point(0, start.y), majorCornerSteps),
						possibleSpacesAfterSteps(traversable, Point(maxX, start.y), majorCornerSteps),
					).sum
			//

			if TABULATE_PARTS then System.out.print(f" (c) $minorCornerSteps%2d $minorCornerSpaces%3d")
			if TABULATE_PARTS then System.out.print(f" (C) $majorCornerSteps%2d $majorCornerSpaces%3d")

			val diamondRadiusInGrids = (steps - 1) / traversable.width
			if TABULATE_PARTS then System.out.print(f" (r) $diamondRadiusInGrids%6d")

			val fullSpaces = locally:
				val polarity = steps % 2
				val notPolarity = 1 - polarity

				// The input has plots that are surrounded by stones;
				// this flood fill grid can be used as a mask to excludes those inaccessible spaces
				// without running `possibleSpacesAfterSteps(traversable, start, traversable.width)`
				val floodFilled = Grid.tabulate(traversable.width, traversable.height):
					Grid
						.fill(traversable.width, traversable.height)(false)
						.explore[Unit, Unit](
							start -> (),
							(_, _) => 0,
							Predef.identity,
							(_, _) => true,
							(p, _) => Direction.values.toSet.filter: dir =>
								val dest = p + dir.toUnitVector
								traversable.isDefinedAt(dest) && traversable(dest)
							,
							(_, _, _, _, _) => (),
						)
						._2
						.map(_._1)



				def fullGridSpaces(polarity: Int): Long =
					Grid
						.tabulate(traversable.width, traversable.height): p =>
							(((p.x + p.y) % 2) == polarity) && floodFilled(p)
						.count: b =>
							b
				val fullGridSpaces0 = fullGridSpaces(0)
				val fullGridSpaces1 = fullGridSpaces(1)
				//*/
				//val fullGridSpaces0 = possibleSpacesAfterSteps(traversable, start, traversable.width + traversable.width % 2)
				//val fullGridSpaces1 = possibleSpacesAfterSteps(traversable, start, traversable.width + traversable.width % 2 + 1)

				val fullGridCount0 = (polarity until diamondRadiusInGrids by 2).map(diagonalPerimeter).sum
				val fullGridCount1 = (notPolarity until diamondRadiusInGrids by 2).map(diagonalPerimeter).sum

				if TABULATE_PARTS then System.out.print(f" (f0) $fullGridSpaces0%3d $fullGridCount0%3d")
				if TABULATE_PARTS then System.out.print(f" (f1) $fullGridSpaces1%3d $fullGridCount1%3d")

				fullGridSpaces0 * fullGridCount0 + fullGridSpaces1 * fullGridCount1
			//

			val minorEdgeSpaces = locally:
				val minorEdgeSteps = (steps - 1) % traversable.width
				val minorEdgeGridCount = (steps - 1) / traversable.width


				val minorEdgeGridSpaces = Seq(
					possibleSpacesAfterSteps(traversable, Point(0, 0), minorEdgeSteps),
					possibleSpacesAfterSteps(traversable, Point(0, maxY), minorEdgeSteps),
					possibleSpacesAfterSteps(traversable, Point(maxX, 0), minorEdgeSteps),
					possibleSpacesAfterSteps(traversable, Point(maxX, maxY), minorEdgeSteps),
				).sum

				if TABULATE_PARTS then System.out.print(f" (e) $minorEdgeGridCount%2d $minorEdgeSteps%2d $minorEdgeGridSpaces%3d")

				minorEdgeGridSpaces * minorEdgeGridCount
			//

			val majorEdgeSpaces = locally:
				val majorEdgeSteps = (steps - 1) % traversable.width + traversable.width
				val majorEdgeGridCount = math.max(0, (steps) / traversable.width - 1)

				val majorEdgeGridSpaces = Seq(
					possibleSpacesAfterSteps(traversable, Point(0, 0), majorEdgeSteps),
					possibleSpacesAfterSteps(traversable, Point(0, maxY), majorEdgeSteps),
					possibleSpacesAfterSteps(traversable, Point(maxX, 0), majorEdgeSteps),
					possibleSpacesAfterSteps(traversable, Point(maxX, maxY), majorEdgeSteps),
				).sum

				if TABULATE_PARTS then System.out.print(f" (E) $majorEdgeGridCount%2d $majorEdgeSteps%2d $majorEdgeGridSpaces%3d")

				majorEdgeGridSpaces * majorEdgeGridCount
			//

			fullSpaces + majorCornerSpaces + minorCornerSpaces + majorEdgeSpaces + minorEdgeSpaces



end possibleSpacesAfterSteps2

/*
 * the actual input has a property where every tile in the same row or column of the starting point is traversable.
 * This is a very convenient feature which the sample input does not have
 * This makes testing against the sample input useless
 * Thus, this sample input instead
 */
def infiniteEmptyFieldSpacesAfterSteps(steps:Int):Long =
	//val polarity = steps % 2
	//(polarity to steps by 2).map(diagonalPerimeter).sum
	(1L + steps) * (1L + steps)
end infiniteEmptyFieldSpacesAfterSteps

object Day21:
	def main(args:Array[String]):Unit =
		val (plots, start) = locally:
			val lines = Grid.fromStrings:
				os.read.lines(os.pwd / "input.txt")
			val start = lines.indexOf('S')
			val plots = lines.map:
				case 'S' | '.' => true
				case '#' => false
			((plots, start))

		System.out.print("part 1: ")
		System.out.println:
			possibleSpacesAfterSteps(plots, start, 64)

		System.out.print("part 2: ")
		//System.out.println()

		/*
		System.out.println("")
		//((0 to 100) :+ 26501365).foreach: steps =>
		((128 to 135) :+ 26501365).foreach: steps =>
			val universeWidth = 131
			System.out.print:
				f"$steps%2d:"
			val _1 = infiniteEmptyFieldSpacesAfterSteps(steps)
			val _2 = possibleSpacesAfterSteps2(Grid.tabulate(universeWidth, universeWidth)(_ => true), Point(universeWidth / 2, universeWidth / 2), steps)
			System.out.println:
				f"    (Exp) $_1%7d (Res) $_2%7d"
		*/
		/*
		System.out.println("")
		(0 to 50).foreach: steps =>
			System.out.print:
				f"$steps%2d: "
			val grid2_2 = Grid:
				Seq(
					Seq(true, true, true, true, true),
					Seq(true, false, true, false, true),
					Seq(true, true, true, true, true),
					Seq(true, false, true, false, true),
					Seq(true, true, true, true, true),
				)
			val grid2 = Grid.tabulate(7, 7): p =>
				p != Point(1,1) && p != Point(2,2)
			val start2 = Point(grid2.width / 2, grid2.height / 2)

			val grid1 = Grid.tabulate(grid2.width * 21, grid2.height * 21): p =>
				grid2(Point(p.x % grid2.width, p.y % grid2.height))
			val start1 = Point(grid1.width / 2, grid1.height / 2)

			val _1 = possibleSpacesAfterSteps(grid1, start1, steps)
			val _2 = possibleSpacesAfterSteps2(grid2, start2, steps)
			System.out.println:
				f"    (Exp) $_1%7d (Res) $_2%7d"
		*/
		/*
		locally:
			val parallelUniverseGrid = Grid.tabulate(plots.width * 11, plots.height * 11): p =>
				plots(Point(p.x % plots.width, p.y % plots.height))
			val parallelUniverseStart = Point(parallelUniverseGrid.width / 2, parallelUniverseGrid.height / 2)

			(132 * 4 / 2 - 3 until 132 * 4 / 2 + 3).foreach: steps =>
				System.out.print:
					f"$steps%2d: "
				val _1 = possibleSpacesAfterSteps(parallelUniverseGrid, parallelUniverseStart, steps)
				val _2 = possibleSpacesAfterSteps2(plots, start, steps)
				System.out.println:
					f"    (Exp) $_1%7d (Res) $_2%7d"
		*/


		//System.out.println()
		val res = possibleSpacesAfterSteps2(plots, start, 26501365)
		//System.out.println()
		System.out.println(res)

