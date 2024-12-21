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

	def moveToCode(to: Point, holeY: Int): Seq[String] =
		if this.x == to.x then
			Seq(moveToCodeVertical(to))
		else if this.y == to.y then
			Seq(moveToCodeHorizontal(to))
		else if this.x == 0 && to.y == holeY then
			Seq(moveToCodeHorizontal(to) + moveToCodeVertical(to))
		else if to.x == 0 && this.y == holeY then
			Seq(moveToCodeVertical(to) + moveToCodeHorizontal(to))
		else
			Seq(
				moveToCodeHorizontal(to) + moveToCodeVertical(to),
				moveToCodeVertical(to) + moveToCodeHorizontal(to),
			)
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

def metacode1(code: String, padPositions: Char => Point, padHoleY: Int): Seq[String] =
	s"A$code"
		.map(padPositions)
		.sliding(2)
		.map: fromTo =>
			val Seq(from, to) = fromTo
			from.moveToCode(to, padHoleY).map(_ + "A")
		.foldLeft(Seq("")): (foldings, nextParts) =>
			for
				folding <- foldings
				nextPart <- nextParts
			yield
				s"$folding$nextPart"

def metacode3(code0: String): String =
	//println(code0)
	val code1s = metacode1(code0, numpadPositions, numpadHole)
	//println(code1s)
	val code2s = code1s.flatMap: code1 =>
		metacode1(code1, dirpadPositions, dirpadHole)
	//println(code2s)
	val code3s = code2s.flatMap: code2 =>
		metacode1(code2, dirpadPositions, dirpadHole)
	//println(code3s)
	code3s.minBy(_.size)

val codes: Seq[String] = os.read.lines(inputFile)

val part1 = codes
	.map: (code:String) =>
		val metacode = metacode3(code)

	//	metacode
	//	s"${metacode.size} * ${code.dropRight(1).toInt}"
		metacode.size * code.dropRight(1).toInt
	.sum

println(s"part 1: $part1")
