//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using file ../../2023/Grid.scala

import name.rayrobdod.aoc.*

enum Direction:
	case N, E, W, S, NE, NW, SE, SW

	def deltaY: Int =
		this match
			case N | NE | NW => -1
			case S | SE | SW => 1
			case E | W => 0

	def deltaX: Int =
		this match
			case E | NE | SE => -1
			case W | NW | SW => 1
			case N | S => 0

	def toVector: Vector =
		Vector(this.deltaX, this.deltaY)

object Day4:
	def main(args: Array[String]): Unit =
		val input: Grid[Char] = Grid.fromStrings(os.read.lines(os.pwd / "input.txt"))

		locally:
			val part1Matches =
				for (
					j <- 0 until input.height;
					i <- 0 until input.width;
					k <- Direction.values
				) yield {
					val root = Point(i, j)
					val k2 = k.toVector

					input.isDefinedAt(root + k2 * 3) &&
						'X' == input(root) &&
						'M' == input(root + k2) &&
						'A' == input(root + k2 * 2) &&
						'S' == input(root + k2 * 3)
				}
			val part1 = part1Matches.count(m => m)

			System.out.println(s"part 1: ${part1}")

		locally:
			val part2Matches =
				for (
					j <- 1 until input.height - 1;
					i <- 1 until input.width - 1
				) yield {
					val root = Point(i, j)
					'A' == input(root) &&
					(
						(
							input(root + Vector(1, 1)) == 'M'
							&&
							input(root + Vector(-1, -1)) == 'S'
						)
						||
						(
							input(root + Vector(1, 1)) == 'S'
							&&
							input(root + Vector(-1, -1)) == 'M'
						)
					)
					&&
					(
						(
							input(root + Vector(1, -1)) == 'M'
							&&
							input(root + Vector(-1, 1)) == 'S'
						)
						||
						(
							input(root + Vector(1, -1)) == 'S'
							&&
							input(root + Vector(-1, 1)) == 'M'
						)
					)
				}
			val part2 = part2Matches.count(m => m)

			System.out.println(s"part 2: ${part2}")
