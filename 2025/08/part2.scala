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

object Day8Part2:
	def main(args: Array[String]): Unit =
		val input: Seq[Point] =
			os.read.lines(os.pwd / "input.txt").map(Point.fromLine)

		val connections =
			(0 until input.size).flatMap: a =>
				((a + 1) until input.size).map: b =>
					((input(a), input(b)))
			.sortBy: (a, b) =>
				a.distanceSq(b)
			.toList

		@annotation.tailrec
		def findLastConnectionRec(connections: List[(Point, Point)], circuits: Seq[Set[Point]]): Long =
			val connection :: nextConnections = connections
			val (aPoint, bPoint) = connection
			val aCircuitIndex = circuits.indexWhere{_.contains(aPoint)}
			val bCircuitIndex = circuits.indexWhere{_.contains(bPoint)}

			val nextCircuits = (aCircuitIndex, bCircuitIndex) match
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

			if nextCircuits.size == 1 && nextCircuits(0).size == input.size then
				aPoint.x * bPoint.x
			else
				findLastConnectionRec(nextConnections, nextCircuits)

		val result = findLastConnectionRec(connections, Seq.empty)

		System.out.println(s"part 2: ${result}")
