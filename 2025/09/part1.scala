//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

case class Point(x: Long, y: Long):
	def sizeOfRectangle(other: Point): Long =
		(1 + math.abs(this.x - other.x)) *
		(1 + math.abs(this.y - other.y))

object Point:
	def fromLine(line: String): Point =
		val parts = line.split(',')
		Point(parts(0).toLong, parts(1).toLong)

object Day9Part1:
	def main(args: Array[String]): Unit =
		val input: Seq[Point] =
			os.read.lines(os.pwd / "input.txt").map(Point.fromLine)

		val result =
			(0 until input.size).flatMap: a =>
				((a + 1) until input.size).map: b =>
					((input(a), input(b)))
			.map: (a, b) =>
				a.sizeOfRectangle(b)
			.max

		System.out.println(s"part 1: ${result}")
