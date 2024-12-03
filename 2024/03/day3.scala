//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

import java.util.regex.*
import scala.util.chaining.scalaUtilChainingOps

object Day3:
	def main(args: Array[String]): Unit =
		val input: String = os.read(os.pwd / "input.txt").replace("\n", " ")

		locally:
			val mulPattern = raw"mul\((\d{1,3}),(\d{1,3})\)".r

			val part1 = mulPattern
				.findAllMatchIn(input)
				.map: m =>
					m.group(1).toInt * m.group(2).toInt
				.sum

			System.out.println(s"part 1: ${part1}")

		locally:
			object I:
				def unapply(s: String): Option[Int] = s.toIntOption

			val Mul = raw"mul\((\d{1,3}),(\d{1,3})\).+".r
			val Enable = raw"do\(\).+".r
			val Disable = raw"don't\(\).+".r

			var part2: Int = 0
			var enabled: Boolean = true

			(0 until input.size).foreach: index =>
				val subinput = input.substring(index, input.size.min(index + 15))

				subinput match
					case Mul(I(a), I(b)) if enabled =>
						part2 += a * b
					case Enable() =>
						enabled = true
					case Disable() =>
						enabled = false
					case _ =>
						// do nothing

			System.out.println(s"part 2: ${part2}")
