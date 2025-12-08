//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

inline def square(x: Long):Long = x * x

case class Point(x: Long, y: Long, z: Long):
	def distanceSq(other: Point): Long =
		square(this.x - other.x) +
		square(this.y - other.y) +
		square(this.z - other.z)

object Point:
	def fromLine(line: String): Point =
		val parts = line.split(',')
		Point(parts(0).toLong, parts(1).toLong, parts(2).toLong)

object Day8Part1:
	def main(args: Array[String]): Unit =
		val input: Seq[Point] =
			os.read.lines(os.pwd / "input.txt").map(Point.fromLine)

		val connections =
			(0 until input.size).flatMap: a =>
				((a + 1) until input.size).map: b =>
					((input(a), input(b)))
			.sortBy: (a, b) =>
				a.distanceSq(b)
			.take(1000)

		val circuits = connections
			.foldLeft[Seq[Set[Point]]](Seq.empty): (circuits, connection) =>
				val (aPoint, bPoint) = connection
				val aCircuitIndex = circuits.indexWhere{_.contains(aPoint)}
				val bCircuitIndex = circuits.indexWhere{_.contains(bPoint)}

				(aCircuitIndex, bCircuitIndex) match
					case (-1, -1) => circuits :+ Set(aPoint, bPoint)
					case (-1, _) => circuits.updated(bCircuitIndex, circuits(bCircuitIndex) + aPoint)
					case (_, -1) => circuits.updated(aCircuitIndex, circuits(aCircuitIndex) + bPoint)
					case _ if aCircuitIndex == bCircuitIndex => circuits
					case _ =>
						val minCircuitIndex = aCircuitIndex.min(bCircuitIndex)
						val maxCircuitIndex = aCircuitIndex.max(bCircuitIndex)
						val newCircuit = circuits(aCircuitIndex) ++ circuits(bCircuitIndex)
						circuits
							.patch(maxCircuitIndex, Nil, 1)
							.patch(minCircuitIndex, Seq(newCircuit), 1)

		val result = circuits
			.map:
				_.size
			.sortBy: sizes =>
				-sizes
			.take(3)
			.product

		System.out.println(s"part 1: ${result}")
