//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2
//> using dep org.typelevel::toolkit:0.1.20
//> using file ../Grid.scala

import cats.Eval
import cats.syntax.all.*
import name.rayrobdod.aoc.*

enum Trail:
	case Path
	case Forest
	case Slope(dir:Direction)

def longestPath(trails: Grid[Trail], start: Point, end: Point): Int =
	import Trail.*
	def rec(current: Point, seen: Set[Point]): Eval[Int] =
		Eval.always(current == end).ifM[Int](
			Eval.always(seen.size),

			trails(current) match
				case Forest => Eval.always(-1)
				case Slope(dir) =>
					val next = current + dir.toUnitVector
					val nextSeen = seen + current
					if seen.contains(next) then
						Eval.always(-1)
					else
						rec(next, nextSeen)
				case Path =>
					val nextSeen = seen + current
					Direction.values
						.map:
							current + _.toUnitVector
						.filter:
							trails.isDefinedAt
						.filterNot:
							seen
						.map: next =>
							rec(next, nextSeen)
						.foldLeft(Eval.always(-1)): (folding, value) =>
							folding.flatMap: a =>
								value.map: b =>
									math.max(a, b)
		)
	end rec

	rec(start, Set.empty).value
end longestPath

object Day23Part1:
	def main(args:Array[String]):Unit =
		val (start, end, trails1, trails2) = locally:
			val lines = os.read.lines(os.pwd / "input.txt")
			val startY = 0
			val startX = lines(startY).indexOf('.')
			val endY = lines.length - 1
			val endX = lines(endY).indexOf('.')
			val trails1 = Grid.fromStrings(lines).map:
				case '.' => Trail.Path
				case '#' => Trail.Forest
				case '>' => Trail.Slope(Direction.Right)
				case '<' => Trail.Slope(Direction.Left)
				case 'v' => Trail.Slope(Direction.Down)
				case '^' => Trail.Slope(Direction.Up)
			val trails2 = trails1.map:
				case Trail.Forest => Trail.Forest
				case _ => Trail.Path
			((Point(startX, startY), Point(endX, endY), trails1, trails2))

		System.out.print("part 1: ")
		System.out.println:
			longestPath(trails1, start, end)

		//System.out.print("part 2: ")
		//System.out.println:
		//	longestPath(trails2, start, end)
