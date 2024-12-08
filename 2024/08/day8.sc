//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using file ../../2023/Grid.scala

import name.rayrobdod.aoc.*

val input_raw: Grid[Char] =
	Grid.fromStrings(os.read.lines(os.pwd / "input.txt"))

val input: Map[Char, Set[Point]] =
	input_raw
		.indices
		.map: (p: Point) =>
			(input_raw(p), p)
		.filter(_._1 != '.')
		.groupMapReduce
			({ (k, _) => k })
			({ (_, v) => Set(v) })
			({ (v1, v2) => v1 ++ v2 })

val part1 = input
	.flatMap[Point]: (k, vs) =>
		for
			left <- vs;
			right <- vs
			if left != right
		yield
			left - (right - left)
	.filter(input_raw.isDefinedAt)
	.to(scala.collection.immutable.HashSet)
	.size

println(s"part 1: ${part1}")

import scala.math.BigInt.int2bigInt

val part2 = input.toList
	.flatMap[(Point, Vector)]: (k, vs) =>
		for
			left <- vs;
			right <- vs
			if left != right
		yield
			(left, (right - left))
	.map: (p, v) =>
		if v.x.gcd(v.y) != 1 then
			throw new Exception(s"${v.x},${v.y}")
		(p, v)
	.flatMap: (p, v) =>
		// the list at this point contains point-vector pairs for both directions,
		// so this only has to concern itself with one direction
		LazyList.unfold(p): p2 =>
			val newP = p2 + v
			Option.when(input_raw.isDefinedAt(newP))((newP, newP))
	.to(scala.collection.immutable.HashSet)
	.size

println(s"part 2: ${part2}")
