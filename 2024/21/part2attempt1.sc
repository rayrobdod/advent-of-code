//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

import scala.util.chaining.scalaUtilChainingOps

val inputFile = os.pwd / "input.txt"

case class Point(x: Int, y: Int):
	private def moveToCodeVertical(to: Point): String =
		if this.y > to.y then
			"v" * (this.y - to.y)
		else
			"^" * (to.y - this.y)
	private def moveToCodeHorizontal(to: Point): String =
		if this.x > to.x then
			"<" * (this.x - to.x)
		else
			">" * (to.x - this.x)

	def moveToCode(to: Point, holeY: Int): String =
		if this.x == to.x then
			moveToCodeVertical(to)
		else if this.y == to.y then
			moveToCodeHorizontal(to)
		else if this.x == 0 && to.y == holeY then
			moveToCodeHorizontal(to) + moveToCodeVertical(to)
		else if to.x == 0 && this.y == holeY then
			moveToCodeVertical(to) + moveToCodeHorizontal(to)
		else if to.y < this.y then
			moveToCodeVertical(to) + moveToCodeHorizontal(to)
		else
			moveToCodeHorizontal(to) + moveToCodeVertical(to)
	end moveToCode
end Point

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
val numpadHole = 0
val dirpadPositions = Map(
	'^' -> Point(1, 1),
	'A' -> Point(2, 1),
	'<' -> Point(0, 0),
	'v' -> Point(1, 0),
	'>' -> Point(2, 0),
)
val dirpadHole = 1

def metacode1(code: String, padPositions: Char => Point, padHoleY: Int): String =
	s"A$code"
		.map(padPositions)
		.sliding(2)
		.flatMap: fromTo =>
			val Seq(from, to) = fromTo
			from.moveToCode(to, padHoleY) + "A"
		.mkString

def metacodeN(code0: String, robots: Int): String =
	val code1 = metacode1(code0, numpadPositions, numpadHole)
	(1 until robots).foldLeft(code1): (metacode, _) =>
		metacode1(metacode, dirpadPositions, dirpadHole)

val codes: Seq[String] = os.read.lines(inputFile)

val part1 = codes
	.map: (code:String) =>
		val metacode = metacodeN(code, 3)
		metacode.size * code.dropRight(1).toInt
	.sum

println(s"part 1: $part1")

val part2 = codes
	.map: (code:String) =>
		val metacode = metacodeN(code, 26)
		metacode.size * code.dropRight(1).toInt
	.sum

println(s"part 2: $part2")
