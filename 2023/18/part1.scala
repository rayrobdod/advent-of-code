//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.annotation.tailrec

case class Vector(x: Int, y: Int):
end Vector

case class Point(x: Int, y: Int):
	def +(v: Vector): Point = Point(this.x + v.x, this.y + v.y)
end Point

enum Direction:
	case Up, Down, Left, Right

	def toUnitVector: Vector =
		this match
			case Up => Vector(0, -1)
			case Down => Vector(0, 1)
			case Left => Vector(-1, 0)
			case Right => Vector(1, 0)
end Direction

object Direction:
	def fromChar(c: Char): Direction =
		c match
			case 'U' => Up
			case 'L' => Left
			case 'D' => Down
			case 'R' => Right

class Grid[A](backing: Seq[Seq[A]]) extends PartialFunction[Point, A]:
	def apply(p: Point): A = backing(p.y)(p.x)

	def isDefinedAt(p: Point): Boolean =
		0 <= p.x && p.x < width && 0 <= p.y && p.y < height

	def updated(p: Point, newValue: A): Grid[A] =
		Grid:
			backing.updated(p.y, backing(p.y).updated(p.x, newValue))

	def map[B](fn: A => B): Grid[B] =
		Grid:
			backing.map: row =>
				row.map:
					fn

	def count(fn: A => Boolean): Int =
		backing
			.map: row =>
				row.count(fn)
			.sum

	def floodFill(using ev: A =:= Boolean): Grid[Boolean] =
		val indexOfABorders = backing.map: line =>
			line.indexOfSlice(Seq(false, true, false))
		val startY = indexOfABorders.indexWhere(_ != -1)
		val startX = indexOfABorders(startY) + 2

		@tailrec def rec(canvas: Grid[Boolean], toVisit: Set[Point]): Grid[Boolean] =
			if toVisit.isEmpty then
				canvas
			else
				val one = toVisit.head
				val nextCanvas = canvas.updated(one, true)
				val addingToVisit = Direction.values
					.map: direction =>
						one + direction.toUnitVector
					.filter:
						canvas.isDefinedAt
					.filterNot:
						canvas
				rec(nextCanvas, (toVisit - one) ++ addingToVisit)
		rec(this.map(ev), Set(Point(startX, startY)))

	def width: Int = backing(0).size
	def height: Int = backing.size
end Grid

object Grid:
	def fromSetAutoOffset(data:Set[Point]): Grid[Boolean] =
		val minX = data.map(_._1).min
		val minY = data.map(_._2).min
		val maxX = data.map(_._1).max
		val maxY = data.map(_._2).max

		Grid:
			Seq.tabulate(maxY - minY + 1, maxX - minX + 1): (y, x) =>
				data.contains(Point(x + minX, y + minY))

case class InputLine(direction:Direction, distance: Int)

def digTrench(instructions: Seq[InputLine]): Set[Point] =
	val initialState = (Point(0, 0), Set.empty[Point])

	instructions
		.foldLeft(initialState): (previousState, instruction) =>
			(0 until instruction.distance).foldLeft(previousState): (previousState, _) =>
				val (previousPosition, previousDugOut) = previousState
				val nextPosition = previousPosition + instruction.direction.toUnitVector
				val nextDugOut = previousDugOut + nextPosition
				(nextPosition, nextDugOut)
		._2
end digTrench

object Day18Part1:
	def main(args:Array[String]):Unit =
		val input: Seq[InputLine] =
			os.read.lines(os.pwd / "input.txt")
				.map:
					case s"$direction $length (#$color)" =>
						InputLine(
							Direction.fromChar(direction.charAt(0)),
							length.toInt,
						)

		System.out.print("part 1: ")
		System.out.println:
			Grid
				.fromSetAutoOffset:
					digTrench(input)
				.floodFill
				.count:
					Predef.identity
