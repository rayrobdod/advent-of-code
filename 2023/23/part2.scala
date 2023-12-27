//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2
//> using file ../Grid.scala

import scala.annotation.tailrec
import name.rayrobdod.aoc.*

enum Trail:
	case Path
	case Forest

case class Segment(start: Point, end: Point, length: Int)

def findIntersections(trails: Grid[Trail]): Set[Point] =
	(0 until trails.height)
		.flatMap: y =>
			(0 until trails.width).flatMap: x =>
				val p = Point(x, y)
				if trails(p) == Trail.Forest then
					Nil
				else
					val numAdjacentPaths =
						Direction.values.toSet
							.map: d =>
								p + d.toUnitVector
							.filter:
								trails.isDefinedAt
							.count: adj =>
								trails(adj) != Trail.Forest
					if numAdjacentPaths > 2 then
						List(p)
					else
						Nil
		.toSet
end findIntersections

def calculateSegments(trails: Grid[Trail], intersections: Set[Point]): Seq[Segment] =
	@tailrec def segment(current: Point, cameFrom: Direction, length: Int): (Point, Int) =
		if intersections.contains(current) then
			(current, length)
		else
			val nextDirs = (Direction.values.toSet - (-cameFrom)).filter: dir =>
				trails(current + dir.toUnitVector) != Trail.Forest
			if nextDirs.size != 1 then throw new Exception(s"$current $nextDirs")
			val nextDir = nextDirs.head
			segment(current + nextDir.toUnitVector, nextDir, length + 1)

	intersections.toSeq
		.flatMap: start =>
			Direction.values
				.filter: dir =>
					val next = start + dir.toUnitVector
					trails.isDefinedAt(next) && trails(next) != Trail.Forest
				.map: dir =>
					val (end, length) = segment(start + dir.toUnitVector, dir, 1)
					Segment(start, end, length)
end calculateSegments

def longestPath(trailSegments: Seq[Segment], start: Point, end: Point): Int =
	val trailSegments2 = trailSegments.groupBy(_.start)

	def rec(current: Point, seen: Set[Point], length: Int): Int =
		if current == end then
			length
		else
			trailSegments2(current)
				.filterNot: segment =>
					seen contains segment.end
				.map: segment =>
					rec(segment.end, seen + current, length + segment.length)
				.maxOption
				.getOrElse(-1)

	rec(start, Set.empty, 0)
end longestPath

object Day23Part2:
	def main(args:Array[String]):Unit =
		val (start, end, trails2) = locally:
			val lines = os.read.lines(os.pwd / "input.txt")
			val startY = 0
			val startX = lines(startY).indexOf('.')
			val endY = lines.length - 1
			val endX = lines(endY).indexOf('.')
			val trails2 = Grid.fromStrings(lines).map:
				case '#' => Trail.Forest
				case _ => Trail.Path
			((Point(startX, startY), Point(endX, endY), trails2))

		System.out.print("part 2: ")
		System.out.println:
			val intersections = findIntersections(trails2) + start + end
			val segments = calculateSegments(trails2, intersections)
			longestPath(segments, start, end)
