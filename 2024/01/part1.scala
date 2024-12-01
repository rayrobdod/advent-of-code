//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

object Day1Part1:
	def main(args: Array[String]):Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val lefts: Seq[Int] =
			lines
				.map:
					line => line.take(5).toInt
				.sorted

		val rights: Seq[Int] =
			lines
				.map:
					line => line.takeRight(5).toInt
				.sorted

		val diffs = lefts.zip(rights)
			.map(_ - _)
			.map(_.abs)

		System.out.println(s"part 1: ${diffs.sum}")
