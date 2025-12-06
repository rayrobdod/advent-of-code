//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

class Range(min:Long, max: Long):
	def contains(value: Long): Boolean =
		min <= value && value <= max

object Day5Part1:
	def main(args: Array[String]): Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val ranges =
			lines
				.takeWhile: line =>
					line != ""
				.map: line =>
					val Array(a, b) = line.split('-')
					new Range(a.toLong, b.toLong)

		val items =
			lines
				.dropWhile: line =>
					line != ""
				.tail
				.map: line =>
					line.toLong

		val result = items.count: item =>
			ranges.exists: range =>
				range.contains(item)

		System.out.println(s"part 1: ${result}")
