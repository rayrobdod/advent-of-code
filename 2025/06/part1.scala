//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

import scala.collection.immutable.ArraySeq

object Day6Part1:
	def main(args: Array[String]): Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val result = lines
			.map: line =>
				ArraySeq.unsafeWrapArray(line.strip.split(" +"))
			.transpose
			.map: problem =>
				problem.last match
					case "*" => problem.init.map(_.toLong).product
					case "+" => problem.init.map(_.toLong).sum
			.sum

		System.out.println(s"part 1: ${result}")
