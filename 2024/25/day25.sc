//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

val inputFile = os.pwd / "input.txt"

val (keys, locks) = locally:
	val input = os.read.lines(inputFile).mkString("\n")
	val lockStrings = input.split("\n\n").toSeq

	lockStrings
		.map: lockString =>
			val lines = lockString.split("\n").toSeq
			val isKey = lines(0) == "....."
			val pins = lines
				.transpose
				.map:
					_
						.count:
							_ == '#'
			(isKey, pins)
		.partitionMap: (isKey, pins) =>
			if isKey then
				Left(pins)
			else
				Right(pins)

val part1 = keys
	.map: key =>
		locks
			.count: lock =>
				key.zip(lock).map(_+_).forall(_ <= 7)
	.sum

println(s"part 1: ${part1}")
