//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep org.typelevel::cats-core:2.12.0

import cats.syntax.all._

val inputFile = os.pwd / "input.txt"

final class Code(private val parts: Map[CodePart, Long]):
	def *(factor: Long) = Code(this.parts.view.mapValues(_ * factor).toMap)
	def |+|(other: Code) = Code(this.parts |+| other.parts)
	def length: Long = this.parts.values.sum

	def metacode: Seq[Code] =
		this.parts
			.foldLeft(Seq(Code.empty)): (foldings, codePartFreq: (CodePart, Long)) =>
				for
					folding <- foldings
					(codePart, frequency) = codePartFreq
					metaCodeParts = codePart.metacode
					metaCodePart <- metaCodeParts
				yield
					folding |+| (metaCodePart * frequency)

	override def equals(other: Any): Boolean =
		other match
			case other2: Code => this.parts == other2.parts
			case _ => false

	override def hashCode: Int =
		31 * this.parts.hashCode

	override def toString: String =
		this.parts
			.view
			.map: (codePart, frequency) =>
				val CodePart(from, to) = codePart
				s"$from$to*$frequency"
			.mkString("Code(", ", ", ")")
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
	def metacode: Seq[Code] =
		dirpadPositions(from)
			.moveToCode(dirpadPositions(to), dirpadHole)
			.map: codeStr =>
				codeStr
					.appended('A')
					.toCode
			.toSeq
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

	def moveToCode(to: Point, holeY: Int): Seq[String] =
		if this.x == to.x then
			Seq(moveToCodeVertical(to))
		else if this.y == to.y then
			Seq(moveToCodeHorizontal(to))
		else if this.x == 0 && to.y == holeY then
			Seq(moveToCodeHorizontal(to) + moveToCodeVertical(to))
		else if to.x == 0 && this.y == holeY then
			Seq(moveToCodeVertical(to) + moveToCodeHorizontal(to))
		else if to.y < this.y then
			Seq(
				moveToCodeVertical(to) + moveToCodeHorizontal(to),
				moveToCodeHorizontal(to) + moveToCodeVertical(to),
			)
		else
			Seq(
				moveToCodeHorizontal(to) + moveToCodeVertical(to),
				//moveToCodeVertical(to) + moveToCodeHorizontal(to),
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

def metacode1(code: String, padPositions: Char => Point, padHoleY: Int): Seq[Code] =
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
		.map:
			_.toCode
		.toSeq

def metacode3(code0: String): Code =
	//println(code0)
	val code1s = metacode1(code0, numpadPositions, numpadHole)
	//println(code1s)
	val code2s = code1s.flatMap: code1 =>
		code1.metacode
	//println(code2s)
	val code3s = code2s.flatMap: code2 =>
		code2.metacode
	code3s.minBy(_.length)

def metacodeN(code0: String, robotsUsingDirectionalKeypad: Int): Code =
	val code1s = metacode1(code0, numpadPositions, numpadHole)
	val codeNs = (0 until robotsUsingDirectionalKeypad).foldLeft(code1s): (codes, _) =>
		val metacodes = codes.flatMap(_.metacode)
		metacodes.filter(_.length == metacodes.map(_.length).min)
	val codeN = codeNs.minBy(_.length)
	println(codeNs.indexOf(codeN))
	println(codeN)
	println(codeNs(0))

	codeN

val codes: Seq[String] = os.read.lines(inputFile)

val part1 = codes
	.map: (code:String) =>
		//val metacode = metacode3(code)
		val metacode = metacodeN(code, 2)
		//s"${metacode.length} * ${code.dropRight(1).toInt}"
		metacode.length * code.dropRight(1).toInt
	.sum

println(s"part 1: $part1")

val part2 = codes
	.map: (code:String) =>
		val metacode = metacodeN(code, 4)
		s"${metacode.length} * ${code.dropRight(1).toInt}"
	//	metacode.length * code.dropRight(1).toInt
	//.sum

println(s"part 2: $part2")
