//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.collection.mutable

enum TypeOfRock:
	case Round, Cube, None

	override def toString: String =
		this match
			case None => "."
			case Cube => "#"
			case Round => "O"

object TypeOfRock:
	def fromChar(c:Char):TypeOfRock =
		c match
			case '.' => None
			case '#' => Cube
			case 'O' => Round

final class Grid(private val backing: Seq[Seq[TypeOfRock]]):
	def slideNorth: Grid =
		val canvas = backing.map:
				_.to(mutable.Seq)

		for
			y <- 0 until canvas.length;
			x <- 0 until canvas(0).length
		do
			if canvas(y)(x) == TypeOfRock.Round then
				var z = y
				while z >= 1 && canvas(z - 1)(x) == TypeOfRock.None do
					canvas(z)(x) = TypeOfRock.None
					canvas(z - 1)(x) = TypeOfRock.Round
					z -= 1

		Grid:
			canvas.map:
				_.to(Seq)

	def slideSouth: Grid =
		val canvas = backing.map:
				_.to(mutable.Seq)

		for
			y <- (canvas.length - 1) to 0 by -1;
			x <- (canvas(0).length - 1) to 0 by -1
		do
			if canvas(y)(x) == TypeOfRock.Round then
				var z = y
				while z < (canvas.length - 1) && canvas(z + 1)(x) == TypeOfRock.None do
					canvas(z)(x) = TypeOfRock.None
					canvas(z + 1)(x) = TypeOfRock.Round
					z += 1

		Grid:
			canvas.map:
				_.to(Seq)

	def slideWest: Grid =
		Grid:
			backing
				.map: line =>
					val canvas = line.to(mutable.Seq)
					(0 until line.length).foreach: x =>
						if canvas(x) == TypeOfRock.Round then
							var z = x
							while z >= 1 && canvas(z - 1) == TypeOfRock.None do
								canvas(z) = TypeOfRock.None
								canvas(z - 1) = TypeOfRock.Round
								z -= 1
					canvas.to(Seq)

	def slideEast: Grid =
		Grid:
			backing
				.map: line =>
					val canvas = line.to(mutable.Seq)
					((line.length - 1) to 0 by -1).foreach: x =>
						if canvas(x) == TypeOfRock.Round then
							var z = x
							while z < (canvas.length - 1) && canvas(z + 1) == TypeOfRock.None do
								canvas(z) = TypeOfRock.None
								canvas(z + 1) = TypeOfRock.Round
								z += 1
					canvas.to(Seq)

	def spinCycle: Grid =
		this
			.slideNorth
			.slideWest
			.slideSouth
			.slideEast

	def longSpin: Grid =
		val TOTAL_CYCLES = 1_000_000_000

		val seen: mutable.Buffer[Grid] = mutable.Buffer.empty
		var current = this

		while ! seen.contains(current) do
			seen += current
			current = current.spinCycle

		val currentIndex = seen.length
		val cycleLength = seen.length - seen.indexOf(current)

		val remaining = (TOTAL_CYCLES - currentIndex) % cycleLength

		(0 until remaining).foreach: _ =>
			current = current.spinCycle

		current

	def northLoad: Int =
		backing.zipWithIndex
			.map: (line, y) =>
				val count = line.count:
					_ == TypeOfRock.Round
				val invertedY = backing.length - y
				count * invertedY
			.sum

	override def toString: String =
		backing
			.map: line =>
				line.mkString
			.mkString("\n")

	override def equals(other: Any): Boolean =
		other match
			case other2: Grid => this.backing == other2.backing
			case _ => false

	override def hashCode: Int =
		this.backing.hashCode

object Day14Part1:
	def main(args:Array[String]):Unit =
		val input =
			Grid:
				os.read.lines(os.pwd / "input.txt")
					.map: line =>
						line.map:
							TypeOfRock.fromChar

		System.out.print("part 1: ")
		System.out.println:
			input.slideNorth.northLoad

		System.out.print("part 2: ")
		System.out.println:
			input.longSpin.northLoad
