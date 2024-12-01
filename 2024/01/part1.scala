//> using scala 3.5.2

import java.nio.file.*

object Day1Part1:
	def main(args: Array[String]):Unit =
		val lefts: List[Int] =
			Files.lines(Path.of("input.txt"))
				.toArray(size => new Array[String](size))
				.toList
				.map:
					line => line.take(5).toInt
				.sorted
		val rights: List[Int] =
			Files.lines(Path.of("input.txt"))
				.toArray(size => new Array[String](size))
				.toList
				.map:
					line => line.takeRight(5).toInt
				.sorted
		val diffs = lefts.zip(rights)
			.map(_ - _)
			.map(_.abs)
		System.out.println(s"part 1: ${diffs.sum}")
