//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2
//> using file ../Grid.scala

import scala.annotation.tailrec
import name.rayrobdod.aoc.*

def nextSpacesAfterStep0(traversable: Grid[Boolean], current: Set[Point]): Set[Point] =
	current
		.flatMap: p =>
			Direction.values
				.map: v =>
					p + v.toUnitVector
				.filter: p =>
					traversable.isDefinedAt(p) && traversable(p)

def spacesAfterSteps(traversable: Grid[Boolean], start: Point, steps: Int): Int =
	(0 until steps)
		.foldLeft(Set(start)): (folding, _) =>
			nextSpacesAfterStep0(traversable, folding)
		.size
end spacesAfterSteps

def spacesAfterSteps2(traversable: Grid[Boolean], steps: Int): Long =
	val modulus = steps % traversable.width
	val dividend = steps / traversable.width

	val parallelUniverseTraversable = Grid.tabulate(traversable.width * 11, traversable.height * 11): p =>
		traversable(Point(p.x % traversable.width, p.y % traversable.height))
	val parallelUniverseStart = Point(parallelUniverseTraversable.width / 2, parallelUniverseTraversable.height / 2)

	val _0 = (0 until modulus)
		.foldLeft(Set(parallelUniverseStart)): (folding, _) =>
			nextSpacesAfterStep0(parallelUniverseTraversable, folding)
	val _1 = (0 until traversable.width)
		.foldLeft(_0): (folding, _) =>
			nextSpacesAfterStep0(parallelUniverseTraversable, folding)
	val _2 = (0 until traversable.width)
		.foldLeft(_1): (folding, _) =>
			nextSpacesAfterStep0(parallelUniverseTraversable, folding)
	/*
	val _3 = (0 until traversable.width)
		.foldLeft(_2): (folding, _) =>
			nextSpacesAfterStep0(parallelUniverseTraversable, folding)
	println(_3.size)

	val sizes = Seq(_0, _1, _2, _3).map(_.size)

	println:
		sizes
	println:
		sizes
			.sliding(2)
			.map:
				case Seq(a,b) => b - a
			.toList
	println:
		sizes
			.sliding(2)
			.map:
				case Seq(a,b) => b - a
			.sliding(2)
			.map:
				case Seq(a,b) => b - a
			.toList
	*/

	val (s0, s1, s2) = (_0.size, _1.size, _2.size)
	//val (s0, s1, s2) = (3734, 33285, 92268)
	val acceleration = (s2 - s1) - (s1 - s0)

	@tailrec def rec(gridStepsRemaining: Int, current: Long, velocity: Long): Long =
		if 0 == gridStepsRemaining then
			current
		else
			rec(
				gridStepsRemaining - 1,
				current + velocity + acceleration,
				velocity + acceleration
			)
	end rec

	rec(dividend - 2, s2, s2 - s1)
end spacesAfterSteps2

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
			spacesAfterSteps(plots, start, 64)

		System.out.print("part 2: ")
		System.out.println:
			spacesAfterSteps2(plots, 26501365)
