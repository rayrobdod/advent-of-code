//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

import scala.util.chaining.scalaUtilChainingOps

val inputFile = os.pwd / "input.txt"

case class Point(x: Int, y: Int):
	def distance(other: Point): Int = (this.x - other.x).abs + (this.y - other.y).abs

	private def dirpadCodeVertical(to: Point, sb:StringBuilder): Unit =
		(this.y until to.y).foreach: _ =>
			sb.append("^")
		(this.y until to.y by -1).foreach: _ =>
			sb.append("v")
	private def dirpadCodeHorizontal(to: Point, sb:StringBuilder): Unit =
		(this.x until to.x).foreach: _ =>
			sb.append(">")
		(this.x until to.x by -1).foreach: _ =>
			sb.append("<")


	def dirpadCode(to: Point, dirpadHole: Point): String =
		val sb = StringBuilder(5)
		// the 'hole' is in the 'x' == 0 column.
		// So, if `this` has x == 0, the horizontal move must happen first,
		// and if `to` has x == 0, the horizontal move must happen last.
		// Ideally we don't have to know what the hole's y coordinate is
		if this.y == dirpadHole.y && to.x == 0 then
			dirpadCodeVertical(to, sb)
			dirpadCodeHorizontal(to, sb)
		else
			dirpadCodeHorizontal(to, sb)
			dirpadCodeVertical(to, sb)
		sb.toString
end Point

val codes: Seq[String] = os.read.lines(inputFile)
//val codes = Seq("2")

val numpadPositions = Map(
	'0' -> Point(1, 0),
	'A' -> Point(2, 0),
	'1' -> Point(0, 1),
	'2' -> Point(1, 1),
	'3' -> Point(2, 1),
	'4' -> Point(0, 2),
	'5' -> Point(1, 2),
	'6' -> Point(2, 2),
	'7' -> Point(0, 3),
	'8' -> Point(1, 3),
	'9' -> Point(2, 3),
)
val numpadHole = Point(0, 0)
val dirpadPositions = Map(
	'^' -> Point(1, 1),
	'A' -> Point(2, 1),
	'<' -> Point(0, 0),
	'v' -> Point(1, 0),
	'>' -> Point(2, 0),
)
val dirpadHole = Point(0, 1)

val part1 = codes
	.map: (code:String) =>
		val thirdPadCode = code
			.tap(println)
			// 0
			.prepended('A')
			.map: c =>
				numpadPositions(c)
			.sliding(2)
			.flatMap: fromTo =>
				val Seq(from, to) = fromTo
				from.dirpadCode(to, numpadHole) + "A"
			.mkString
			.tap(println)
			// 1
			.prepended('A')
			.map: c =>
				dirpadPositions(c)
			.sliding(2)
			.flatMap: fromTo =>
				val Seq(from, to) = fromTo
				from.dirpadCode(to, dirpadHole) + "A"
			.mkString
			.tap(println)
			// 2
			.prepended('A')
			.map: c =>
				dirpadPositions(c)
			.sliding(2)
			.flatMap: fromTo =>
				val Seq(from, to) = fromTo
				from.dirpadCode(to, dirpadHole) + "A"
			.mkString
			.tap(println)
			// 3


		//s"${thirdPadCode.size} * ${code.dropRight(1).toInt}"
		thirdPadCode.size * code.dropRight(1).toInt
	.sum

println(s"part 1: $part1")
