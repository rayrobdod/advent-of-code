//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.annotation.tailrec
import scala.collection.immutable.BitSet

final case class Coordinate(x:Int, y:Int):
	def max(that: Coordinate): Coordinate =
		Coordinate(
			math.max(this.x, that.x),
			math.max(this.y, that.y),
		)

object Coordinate:
	def zero: Coordinate = Coordinate(0, 0)

object Day11:
	def main(args:Array[String]):Unit =
		val preexpansion:Seq[Coordinate] = os.read.lines(os.pwd / "input.txt")
			.zipWithIndex
			.flatMap: (line, rowIndex) =>
				line.zipWithIndex
					.collect:
						case ('#', columnIndex) => Coordinate(rowIndex, columnIndex)

		val size = preexpansion.foldLeft(Coordinate.zero):
			_ max _

		val xsWithoutGalaxies: Set[Int] =
			BitSet.fromSpecific(0 to size.x) -- preexpansion.map(_.x).to(BitSet)

		val ysWithoutGalaxies: Set[Int] =
			BitSet.fromSpecific(0 to size.y) -- preexpansion.map(_.y).to(BitSet)

		def distance(from: Coordinate, to: Coordinate, expansionFactor: Int): Long =
			val preexpansionRows = math.abs(from.x - to.x)
			val preexpansionCols = math.abs(from.y - to.y)
			val expansionRows = xsWithoutGalaxies.count:
				x => (from.x < x && x < to.x) || (to.x < x && x < from.x)
			val expansionCols = ysWithoutGalaxies.count:
				y => (from.y < y && y < to.y) || (to.y < y && y < from.y)

			preexpansionRows + preexpansionCols + expansionRows * expansionFactor + expansionCols * expansionFactor

		val distancesPart1: Seq[Long] =
			for (
				from <- preexpansion;
				to <- preexpansion
			) yield
				distance(from, to, 1)

		val distancesPart2: Seq[Long] =
			for (
				from <- preexpansion;
				to <- preexpansion
			) yield {
				distance(from, to, 999_999)
			}

		System.out.println(s"part 1: ${distancesPart1.sum / 2}")
		System.out.println(s"part 2: ${distancesPart2.sum / 2}")

