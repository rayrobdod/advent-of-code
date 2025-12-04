//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6
//> using file ../../2023/Grid.scala

import name.rayrobdod.aoc.*

object Day4Part1:
	def main(args: Array[String]): Unit =
		val input: Grid[Char] = Grid.fromStrings(os.read.lines(os.pwd / "input.txt"))

		val result = input
			.indices
			.filter: i =>
				input(i) == '@'
			.count: i =>
				(
					for
						dy <- -1 to 1;
						dx <- -1 to 1
						if dx != 0 || dy != 0;
						v = Vector(dx, dy)
					yield
						input.getOrElse(i + v, ' ')
				)
				.count(_ == '@') < 4

		System.out.println(s"part 1: ${result}")
