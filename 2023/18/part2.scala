//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.collection.immutable.SortedSet

case class Point(x: Int, y: Int):
end Point

enum Direction:
	case Up, Down, Left, Right
end Direction

object Direction:
	def fromChar(c: Char): Direction =
		c match
			case 'U' => Up
			case 'L' => Left
			case 'D' => Down
			case 'R' => Right
			case '0' => Right
			case '1' => Down
			case '2' => Left
			case '3' => Up
end Direction

case class HorizontalTrenchLine(x:Int, minY: Int, maxY: Int):
	if (maxY <= minY)
		throw new IllegalArgumentException(s"maxY ($maxY) <= minY ($minY)")

	def intersectsY(y: Int): Boolean =
		minY <= y && y <= maxY
end HorizontalTrenchLine

object HorizontalTrenchLine:
	given Ordering[HorizontalTrenchLine] =
		Ordering.by[HorizontalTrenchLine, Int](_.x).orElseBy(_.minY).orElseBy(_.maxY)
end HorizontalTrenchLine

def digTrenchLines(instruction: Seq[Instruction]): SortedSet[HorizontalTrenchLine] =
	import Direction.*
	instruction
		.foldLeft((Point(0, 0), SortedSet.empty[HorizontalTrenchLine])): (folding, instruction) =>
			val (previousPosition, previousTrenchLines) = folding
			instruction.direction match
				case Left =>
					val Point(prevX, y) = previousPosition
					val nextX = prevX - instruction.distance
					((Point(nextX, y), previousTrenchLines))
				case Right =>
					val Point(prevX, y) = previousPosition
					val nextX = prevX + instruction.distance
					((Point(nextX, y), previousTrenchLines))
				case Up =>
					val Point(x, prevY) = previousPosition
					val nextY = prevY - instruction.distance
					((Point(x, nextY), previousTrenchLines + HorizontalTrenchLine(x, nextY, prevY)))
				case Down =>
					val Point(x, prevY) = previousPosition
					val nextY = prevY + instruction.distance
					((Point(x, nextY), previousTrenchLines + HorizontalTrenchLine(x, prevY, nextY)))
		._2
end digTrenchLines

def digInterior(trenchLines: SortedSet[HorizontalTrenchLine]): Long =
	val minY = trenchLines.minBy(_.minY).minY
	val maxY = trenchLines.maxBy(_.maxY).maxY

	enum FillRuleState:
		case Outside
		case Inside(start:Int)
		/** UpInside means the north side is inside the shape and the south side is outside */
		case UpInside(start:Int)
		case DownInside(start:Int)
	import FillRuleState.*

	(minY to maxY)
		.map: y =>
			val relevantTrenchLines = trenchLines.filter(_.intersectsY(y))
			relevantTrenchLines
				.foldLeft(Outside, List.empty[(Int, Int)]): (folding, trench) =>
					val (fillState, segments) = folding
					fillState match
						case Outside if trench.minY == y =>
							(DownInside(trench.x), segments)
						case Outside if trench.maxY == y =>
							(UpInside(trench.x), segments)
						case Outside =>
							(Inside(trench.x), segments)
						case Inside(minX) if trench.minY == y =>
							(UpInside(minX), segments)
						case Inside(minX) if trench.maxY == y =>
							(DownInside(minX), segments)
						case Inside(minX) =>
							(Outside, (minX, trench.x) :: segments)
						case UpInside(minX) if trench.minY == y =>
							(Inside(minX), segments)
						case UpInside(minX) if trench.maxY == y =>
							(Outside, (minX, trench.x) :: segments)
						case UpInside(minX) =>
							throw new AssertionError("")
						case DownInside(minX) if trench.minY == y =>
							(Outside, (minX, trench.x) :: segments)
						case DownInside(minX) if trench.maxY == y =>
							(Inside(minX), segments)
						case DownInside(minX) =>
							throw new AssertionError("")
				._2
				.map: (minX, maxX) =>
					maxX - minX + 1L
				.sum
		.sum
end digInterior

case class Instruction(direction:Direction, distance: Int)

object Day18:
	def main(args:Array[String]):Unit =
		val (input1, input2) =
			os.read.lines(os.pwd / "input.txt")
				.map:
					case s"$direction $length (#$color)" =>
						val p1 = Instruction(
							Direction.fromChar(direction.charAt(0)),
							length.toInt,
						)
						val p2 = Instruction(
							Direction.fromChar(color.charAt(5)),
							Integer.parseInt(color.substring(0, 5), 16)
						)
						((p1, p2))
				.unzip

		System.out.print("part 1: ")
		System.out.println:
			digInterior:
				digTrenchLines:
					input1

		System.out.print("part 2: ")
		System.out.println:
			digInterior:
				digTrenchLines:
					input2
