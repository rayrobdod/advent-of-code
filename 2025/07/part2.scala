//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

object Day7Part2:
	def main(args: Array[String]): Unit =
		val input: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val start = input(0).indexOf('S')

		val result = input.tail
			.foldLeft[Map[Int, Long]](Map(start -> 1)): (beams, line) =>
				beams
					.toSeq
					.flatMap: beam =>
						if line(beam._1) == '^' then
							Seq(beam._1 - 1 -> beam._2, beam._1 + 1 -> beam._2 )
						else
							Seq(beam)
					.foldLeft(Map.empty[Int, Long]): (folding, beam) =>
						folding.updatedWith(beam._1)({(x: Option[Long]) => Option[Long](x.getOrElse(0L) + beam._2)})
			.toSeq
			.map:
				_._2
			.sum

		System.out.println(s"part 2: ${result}")
