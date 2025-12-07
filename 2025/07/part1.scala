//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

object Day7Part1:
	def main(args: Array[String]): Unit =
		val input: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val start = input(0).indexOf('S')

		val result = input.tail
			.foldLeft[(Set[Int], Int)]((Set(start), 0)): (folding, line) =>
				val (beams, count) = folding

				val newBeams = beams.flatMap: beam =>
					if line(beam) == '^' then
						Set(beam - 1, beam + 1)
					else
						Set(beam)
				val newCount = beams.count{beam => line(beam) == '^'}

				(newBeams, count + newCount)
			._2

		System.out.println(s"part 1: ${result}")
