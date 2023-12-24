//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

case class Point(x: Long, y: Long)

case class Velocity(x: Int, y: Int)

enum Intersection:
	case None
	case Some(x: Double, y: Double)

case class Hailstone(p: Point, v: Velocity):
	val slope: Double = v.y.toDouble / v.x
	val y0: Double = p.y - p.x * slope

	def intersection(other: Hailstone): Intersection =
		if this.slope == other.slope then
			Intersection.None
		else
			val x = (this.y0 - other.y0) / (other.slope - this.slope)
			val y = this.y0 + (x * this.slope)
			Intersection.Some(x, y)
	end intersection

	def intersectsInFutureInArea(other: Hailstone, areaMin: Double, areaMax: Double): Boolean =
		this.intersection(other) match
			case Intersection.None => false
			case Intersection.Some(x, y) =>
				areaMin < x && x < areaMax &&
				areaMin < y && y < areaMax &&
				(x - this.p.x).sign == (this.v.x).sign &&
				(x - other.p.x).sign == (other.v.x).sign
end Hailstone


object Day24Part1:
	def main(args:Array[String]):Unit =
		val input = os.read.lines(os.pwd / "input.txt")
			.map:
				case s"$x1, $y1, $z1 @ $x2, $y2, $z2" =>
					Hailstone(Point(x1.strip.toLong, y1.strip.toLong), Velocity(x2.strip.toInt, y2.strip.toInt))

		val areaMin = 200000000000000.0
		val areaMax = 400000000000000.0

		print("part 1: ")
		println:
			input.zipWithIndex
				.map: (_1, idx1) =>
					input
						.drop(idx1 + 1)
						.count: _2 =>
							_1.intersectsInFutureInArea(_2, areaMin, areaMax)
				.sum
