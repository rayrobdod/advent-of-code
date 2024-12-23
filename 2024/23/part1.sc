//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep org.scala-lang.modules::scala-collection-contrib:0.4.0

import scala.collection.immutable.*

val inputFile = os.pwd / "input.txt"

val connections: MultiDict[String, String] =
	os.read.lines(inputFile)
		.flatMap:
			case s"${x}-${y}" => Seq((x, y), (y, x))
		.to(MultiDict)

val computers: Set[String] = connections.keySet.to(Set)

val computersStartingWithT: Set[String] = computers.filter(_.charAt(0) == 't')

val triplesWithT: Set[Set[String]] =
	for
		_1 <- computersStartingWithT
		_2 <- connections.sets(_1)
		_3 <- connections.sets(_1)
		if connections.containsEntry(_2, _3)
	yield
		Set(_1, _2, _3)
	end for

println(s"part 1: ${triplesWithT.size}")
