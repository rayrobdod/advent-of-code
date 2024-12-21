//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep org.typelevel::cats-core:2.12.0

import scala.util.chaining.scalaUtilChainingOps
import cats.syntax.all._

val inputFile = os.pwd / "input.txt"

final class Code(private val parts: Map[CodePart, Long]):
	def *(factor: Long) = Code(this.parts.view.mapValues(_ * factor).toMap)
	def |+|(other: Code) = Code(this.parts |+| other.parts)
	def length: Long = this.parts.values.sum

	def metacode: Code =
		this.parts.foldLeft(Code.empty): (folding, codePartFreq) =>
			val (codePart, frequency) = codePartFreq
			folding |+| (codePart.metacode * frequency)
end Code

object Code:
	def empty: Code = Code(Map.empty)

	def fromString(str: String): Code =
		Code:
			str
				.prepended('A')
				.sliding(2)
				.map: fromTo =>
					val from = fromTo.charAt(0)
					val to = fromTo.charAt(1)
					CodePart(from, to)
				.toSeq
				.groupMapReduce(k => k)(v => 1L)(_+_)
end Code

extension (self: String) def toCode = Code.fromString(self)

case class CodePart(from: Char, to: Char):
	def metacode: Code =
		dirpadPositions(from)
			.moveToCode(dirpadPositions(to), dirpadHole)
			.appended('A')
			.toCode
end CodePart

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

def metacodeN(code0: String, robotsUsingDirectionalKeypad: Int): Code =
	val code1Str = metacode1(code0, numpadPositions, numpadHole)
	val code1 = code1Str.toCode
	(0 until robotsUsingDirectionalKeypad).foldLeft(code1): (code, _) =>
		code.metacode

val codes: Seq[String] = os.read.lines(inputFile)

val part1 = codes
	.map: (code:String) =>
		val metacode = metacodeN(code, 2)
		//s"${metacode.view.values.sum} * ${code.dropRight(1).toInt}"
		metacode.length * code.dropRight(1).toInt
	.sum

println(s"part 1: $part1")

val part2 = codes
	.map: (code:String) =>
		val metacode = metacodeN(code, 25)
		metacode.length * code.dropRight(1).toInt
	.sum

println(s"part 2: $part2")
