//> using scala 3.3.1

object Day9Part1:
	def findNextValue(in:Seq[Int]):Int =
		if in.sizeIs == 1 then
			in.last
		else
			val diffs = in.zip(in.tail).map:
				(a, b) => b - a
			val nextDiff = findNextValue(diffs)
			in.last + nextDiff

	def findPreviousValue(in:Seq[Int]):Int =
		if in.sizeIs == 1 then
			in.last
		else
			val diffs = in.zip(in.tail).map:
				(a, b) => b - a
			val nextDiff = findPreviousValue(diffs)
			in.head - nextDiff

	def main(args:Array[String]):Unit =
		import java.nio.file.*
		val part1Result = Files.lines(Path.of("input.txt"))
			.map:
				line => line.split("\\s+").map(_.toInt).toSeq
			.mapToInt:
				findNextValue
			.sum
		System.out.println(s"part 1: $part1Result")

		val part2Result = Files.lines(Path.of("input.txt"))
			.map:
				line => line.split("\\s+").map(_.toInt).toSeq
			.mapToInt:
				findPreviousValue
			.sum
		System.out.println(s"part 2: $part2Result")
