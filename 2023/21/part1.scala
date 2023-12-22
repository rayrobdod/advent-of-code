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
