//> using scala 3.5.2

import java.nio.file.*

object Day1Part2:
	def main(args: Array[String]):Unit =
		val lefts: List[Int] =
			Files.lines(Path.of("input.txt"))
				.toArray(size => new Array[String](size))
				.toList
				.map:
					line => line.take(5).toInt

		val rights: Map[Int, Int] =
			Files.lines(Path.of("input.txt"))
				.toArray(size => new Array[String](size))
				.toList
				.map:
					line => line.takeRight(5).toInt
				.groupBy({(x: Int) => x})
				.map({(k, v) => (k, v.size)})

		val result = lefts
			.map:
				x => x * rights.getOrElse(x, 0)
			.sum

		System.out.println(s"part 2: ${result}")
