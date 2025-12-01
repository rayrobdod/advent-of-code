//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

object Day1Part1:
	def main(args: Array[String]): Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val finish = lines.foldLeft[(Int, Int)]((50, 0)): (posAndNumZeros, line) =>
			val (pos, zeros) = posAndNumZeros
			val newPos1 = line match
				case s"L$distance" => pos - distance.toInt
				case s"R$distance" => pos + distance.toInt
			val newPos = (((newPos1 % 100) + 100) % 100)
			((newPos, zeros + (if (0 == newPos) {1} else {0})))

		System.out.println(s"part 1: ${finish._2}")
