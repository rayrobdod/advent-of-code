//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

object Day1Part2:
	def main(args: Array[String]): Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val finish = lines.foldLeft[(Int, Int)]((50, 0)): (posAndNumZeros, line) =>
			val (pos, zeros) = posAndNumZeros
			val distance = line match
				case s"L$distance" => - distance.toInt
				case s"R$distance" => + distance.toInt

			val rotations = math.abs(distance / 100)
			val partial = distance % 100

			val newPos1 = pos + partial
			val partialCrossesZero = ((newPos1 < 1) || (newPos1 > 99)) && (pos != 0)
			val newPos = (((newPos1 % 100) + 100) % 100)

			val newZeros = zeros + rotations + (if (partialCrossesZero) {1} else {0})

			((newPos, newZeros))

		System.out.println(s"part 2: ${finish._2}")
