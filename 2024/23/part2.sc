//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep org.scala-lang.modules::scala-collection-contrib:0.4.0

import scala.collection.immutable.*

val inputFile = os.pwd / "input.txt"

val pairs: MultiDict[String, String] =
	os.read.lines(inputFile)
		.flatMap:
			case s"${x}-${y}" => Seq((x, y), (y, x))
		.to(MultiDict)

// Experimentally, all computers have 13 connections
// Guess that the most connected group has all 14 computers fully connected

val fullyConnectedGroup = pairs.sets.find: kvs =>
	val (_1, vs) = kvs
	vs.forall: _2 =>
		vs.forall: _3 =>
			_2 == _3 || pairs.containsEntry((_2, _3))

println(fullyConnectedGroup) // -> None. So the guess is wrong



val triples: MultiDict[(String, String), String] =
	pairs
		.flatMap: (_1, _2) =>
			val Seq(_1a, _2a) = Seq(_1, _2).sorted

			pairs.sets(_1).intersect(pairs.sets(_2)).map: _3 =>
				(((_1a, _2a), _3))
end triples

println(s"triples: ${triples.size}")

val quads: MultiDict[(String, String, String), String] =
	triples
		.flatMap: kv =>
			val ((_1, _2), _3) = kv
			val Seq(_1a, _2a, _3a) = Seq(_1, _2, _3).sorted

			pairs.sets(_1).intersect(pairs.sets(_2)).intersect(pairs.sets(_3)).map: _4 =>
				(((_1a, _2a, _3a), _4))
end quads

println(s"quads: ${quads.size}")

val quints: MultiDict[(String, String, String, String), String] =
	quads
		.flatMap: kv =>
			val ((_1, _2, _3), _4) = kv
			val Seq(_1a, _2a, _3a, _4a) = Seq(_1, _2, _3, _4).sorted

			pairs.sets(_1)
				.intersect(pairs.sets(_2))
				.intersect(pairs.sets(_3))
				.intersect(pairs.sets(_4))
				.map: _5 =>
					(((_1a, _2a, _3a, _4a), _5))
end quints

println(s"quints: ${quints.size}")

val hexes: MultiDict[(String, String, String, String, String), String] =
	quints
		.flatMap: kv =>
			val (k, _5) = kv
			val ((_1, _2, _3, _4)) = k
			val Seq(_1a, _2a, _3a, _4a, _5a) = Seq(_1, _2, _3, _4, _5).sorted

			quints.sets(k)
				.intersect(pairs.sets(_5))
				.map: _6 =>
					(((_1a, _2a, _3a, _4a, _5a), _6))
end hexes

println(s"hexes: ${hexes.size}")

val septs: MultiDict[(String, String, String, String, String, String), String] =
	hexes
		.flatMap: kv =>
			val (k, _6) = kv
			val ((_1, _2, _3, _4, _5)) = k
			val Seq(_1a, _2a, _3a, _4a, _5a, _6a) = Seq(_1, _2, _3, _4, _5, _6).sorted

			hexes.sets(k)
				.intersect(pairs.sets(_6))
				.map: _7 =>
					(((_1a, _2a, _3a, _4a, _5a, _6a), _7))
end septs

println(s"septs: ${septs.size}")

val octs: MultiDict[(String, String, String, String, String, String, String), String] =
	septs
		.flatMap: kv =>
			val (k, _7) = kv
			val ((_1, _2, _3, _4, _5, _6)) = k
			val Seq(_1a, _2a, _3a, _4a, _5a, _6a, _7a) = Seq(_1, _2, _3, _4, _5, _6, _7).sorted

			septs.sets(k)
				.intersect(pairs.sets(_7))
				.map: _8 =>
					(((_1a, _2a, _3a, _4a, _5a, _6a, _7a), _8))
end octs

println(s"octs: ${octs.size}")

val nons: MultiDict[(String, String, String, String, String, String, String, String), String] =
	octs
		.flatMap: kv =>
			val (k, _8) = kv
			val ((_1, _2, _3, _4, _5, _6, _7)) = k
			val Seq(_1a, _2a, _3a, _4a, _5a, _6a, _7a, _8a) = Seq(_1, _2, _3, _4, _5, _6, _7, _8).sorted

			octs.sets(k)
				.intersect(pairs.sets(_8))
				.map: _9 =>
					(((_1a, _2a, _3a, _4a, _5a, _6a, _7a, _8a), _9))
end nons

println(s"nons: ${nons.size}")

val decs: MultiDict[(String, String, String, String, String, String, String, String, String), String] =
	nons
		.flatMap: kv =>
			val (k, _9) = kv
			val ((_1, _2, _3, _4, _5, _6, _7, _8)) = k
			val Seq(_1a, _2a, _3a, _4a, _5a, _6a, _7a, _8a, _9a) = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9).sorted

			nons.sets(k)
				.intersect(pairs.sets(_9))
				.map: _10 =>
					(((_1a, _2a, _3a, _4a, _5a, _6a, _7a, _8a, _9a), _10))
end decs

println(s"decs: ${decs.size}")

val _11s: MultiDict[(String, String, String, String, String, String, String, String, String, String), String] =
	decs
		.flatMap: kv =>
			val (k, _10) = kv
			val ((_1, _2, _3, _4, _5, _6, _7, _8, _9)) = k
			val Seq(_1a, _2a, _3a, _4a, _5a, _6a, _7a, _8a, _9a, _10a) = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10).sorted

			decs.sets(k)
				.intersect(pairs.sets(_10))
				.map: _11 =>
					(((_1a, _2a, _3a, _4a, _5a, _6a, _7a, _8a, _9a, _10a), _11))
end _11s

println(s"_11s: ${_11s.size}")

val _12s: MultiDict[(String, String, String, String, String, String, String, String, String, String, String), String] =
	_11s
		.flatMap: kv =>
			val (k, _11) = kv
			val ((_1, _2, _3, _4, _5, _6, _7, _8, _9, _10)) = k
			val Seq(_1a, _2a, _3a, _4a, _5a, _6a, _7a, _8a, _9a, _10a, _11a) = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11).sorted

			_11s.sets(k)
				.intersect(pairs.sets(_11))
				.map: _12 =>
					(((_1a, _2a, _3a, _4a, _5a, _6a, _7a, _8a, _9a, _10a, _11a), _12))
end _12s

println(s"_12s: ${_12s.size}")

val _13s: MultiDict[(String, String, String, String, String, String, String, String, String, String, String, String), String] =
	_12s
		.flatMap: kv =>
			val (k, _12) = kv
			val ((_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11)) = k
			val Seq(_1a, _2a, _3a, _4a, _5a, _6a, _7a, _8a, _9a, _10a, _11a, _12a) = Seq(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12).sorted

			_12s.sets(k)
				.intersect(pairs.sets(_12))
				.map: _13 =>
					(((_1a, _2a, _3a, _4a, _5a, _6a, _7a, _8a, _9a, _10a, _11a, _12a), _13))
end _13s

println(s"_13s: ${_13s.size}")

println:
	_13s
		.map: kv =>
			val ((_2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13), _1) = kv
			SortedSet(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13)
		.head
		.mkString(",")
