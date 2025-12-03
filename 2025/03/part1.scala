//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

object Day3Part1:
	def main(args: Array[String]): Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val result = lines
			.map: line =>
				val digit1 = line.init.max
				val digit1idx = line.indexOf(digit1)
				val digit2 = line.substring(digit1idx + 1).max
				s"$digit1$digit2"
			.map: x =>
				x.toInt
			.sum

		System.out.println(s"part 1: ${result}")
