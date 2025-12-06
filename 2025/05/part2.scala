//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

case class Range(min:Long, max: Long):
	def fullyContains(other:Range): Boolean = this.min <= other.min && this.max >= other.max
	def size: Long = max - min + 1

	override def toString: String = String.format("%16d-%16d", min, max)

object Range:
	given Ordering[Range] = Ordering.by[Range, Long](_.min)

object Day5Part1:
	def main(args: Array[String]): Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val result =
			lines
				.takeWhile: line =>
					line != ""
				.map: line =>
					val Array(a, b) = line.split('-')
					new Range(a.toLong, b.toLong)
				.sorted
				.foldLeft(List.empty[Range]): (folding, item) =>
					folding match
						case head :: tail if head.fullyContains(item) => head :: tail
						case head :: tail if item.min <= head.max => Range(head.min, item.max) :: tail
						case _ => item :: folding
				.map:
					_.size
				.sum

		//result.foreach: range =>
		//	System.out.println(range)

		System.out.println(s"part 2: ${result}")
