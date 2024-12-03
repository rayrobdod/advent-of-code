//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

import java.util.regex.*
import scala.util.chaining.scalaUtilChainingOps

object Day3:
	def main(args: Array[String]): Unit =
		val mulPattern = Pattern.compile(raw"mul\((\d{1,3}),(\d{1,3})\)")
		val enablePattern = Pattern.compile(raw"do\(\).+")
		val disablePattern = Pattern.compile(raw"don't\(\)")

		val input: String = os.read(os.pwd / "input.txt").replace("\n", " ")

		locally:
			val mulPattern = Pattern.compile(raw"mul\((\d{1,3}),(\d{1,3})\)")

			var part1: Int = 0

			val matcher = mulPattern.matcher(input)
			while matcher.find() do
				part1 += matcher.group(1).toInt * matcher.group(2).toInt
			end while

			System.out.println(s"part 1: ${part1}")

		locally:
			val mulPattern = Pattern.compile(raw"mul\((\d{1,3}),(\d{1,3})\).+")
			val enablePattern = Pattern.compile(raw"do\(\).+")
			val disablePattern = Pattern.compile(raw"don't\(\).+")

			var part2: Int = 0
			var enabled: Boolean = true

			(0 until input.size).foreach: index =>
				val subinput = input.substring(index, input.size.min(index + 15))
				val mulMatcher = mulPattern.matcher(subinput)

				if enabled && mulMatcher.matches() then
					part2 += mulMatcher.group(1).toInt * mulMatcher.group(2).toInt
				else if enablePattern.matcher(subinput).matches() then
					enabled = true
				else if disablePattern.matcher(subinput).matches() then
					enabled = false
				end if

			System.out.println(s"part 2: ${part2}")
