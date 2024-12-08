//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using file ../../2023/Grid.scala

import name.rayrobdod.aoc.*

val NOT_AN_ANTENNA = '.'

val input_raw: Grid[Char] =
	Grid.fromStrings(os.read.lines(os.pwd / "input.txt"))

// The Point is the location of an antenna.
// The Vector is distance from that antenna to a resonant antenna.
// The seq contains both directions for each antenna pair
val input: Seq[(Point, Vector)] =
	input_raw
		.indices
		.map: (p: Point) =>
			(input_raw(p), p)
		.filterNot(_._1 == NOT_AN_ANTENNA)
		.groupMapReduce
			({ (k, _) => k })
			({ (_, v) => Set(v) })
			({ (v1, v2) => v1 ++ v2 })
		// type of antenna -> set of points with that type of antenna
		.map(_._2)
		.toSeq
		.flatMap[(Point, Vector)]: (resonantAntennas) =>
			for
				left <- resonantAntennas;
				right <- resonantAntennas
				if left != right
			yield
				(left, (right - left))

val part1 = input
	.map: (antenna, delta) =>
		antenna + delta * 2
	.filter(input_raw.isDefinedAt)
	.distinct
	.size

println(s"part 1: ${part1}")

import scala.math.BigInt.int2bigInt // gcd

val part2 = input.toList
	.map: (antenna, delta) =>
		if delta.x.gcd(delta.y) != 1 then
			throw new Exception(s"${delta.x},${delta.y}")
		(antenna, delta)
	.flatMap: (antenna, delta) =>
		// the list contains point-vector pairs for both directions,
		// so this only has to concern itself with one direction
		LazyList.unfold(antenna): p =>
			val newP = p + delta
			Option.when(input_raw.isDefinedAt(newP))((newP, newP))
	.distinct
	.size

println(s"part 2: ${part2}")
