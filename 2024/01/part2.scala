//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

object Day1Part2:
	def main(args: Array[String]):Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val lefts: Seq[Int] =
			lines
				.map:
					line => line.take(5).toInt

		val rights: Map[Int, Int] =
			lines
				.map:
					line => line.takeRight(5).toInt
				.groupBy({(x: Int) => x})
				.map({(k, v) => (k, v.size)})

		val result = lefts
			.map:
				x => x * rights.getOrElse(x, 0)
			.sum

		System.out.println(s"part 2: ${result}")
