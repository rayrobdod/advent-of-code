//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6
//> using file ../../2023/Grid.scala

import name.rayrobdod.aoc.*

object Day4Part2:
	def removeRolls(input: Grid[Char]): (Grid[Char], Int) =
		val removedIndicies = input
			.indices
			.filter: i =>
				input(i) == '@'
			.filter: i =>
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

		val retval = removedIndicies.foldLeft(input){_.updated(_, 'x')}
		((retval, removedIndicies.size))

	@annotation.tailrec
	def removeRollsRec(input: Grid[Char], countSoFar: Int = 0): Int =
		val (next, count) = removeRolls(input)
		val nextCount = countSoFar + count
		if input == next then
			nextCount
		else
			removeRollsRec(next, nextCount)

	def main(args: Array[String]): Unit =
		val input: Grid[Char] = Grid.fromStrings(os.read.lines(os.pwd / "input.txt"))

		val result = removeRollsRec(input)

		System.out.println(s"part 2: ${result}")
