//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

object Day3Part2:
	def main(args: Array[String]): Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val result = lines
			.map: line =>
				(11 to 0 by -1)
					.foldLeft(("", 0)): (folding, right) =>
						val (str, left) = folding
						val digit:Char = line.substring(left, line.size - right).max
						val digitidx = line.indexOf(digit, left) + 1
						((s"$str$digit", digitidx))
					._1
			.map: x =>
				x.toLong
			.sum

		System.out.println(s"part 2: ${result}")
