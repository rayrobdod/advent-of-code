//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

val inputFile = os.pwd / "input.txt"

val (avaliables: List[String], desireds: List[String]) =
	val split = os.read(inputFile).split("\n\n")
	(split(0).split(", ").toList, split(1).linesIterator.toList)

val part1 = desireds
	.filter: desired =>
		def impl(remainingDesired: String): Boolean =
			if "" == remainingDesired then
				true
			else
				avaliables
					.filter: avaliable =>
						remainingDesired.startsWith(avaliable)
					.find: avaliable =>
						impl(remainingDesired.drop(avaliable.size))
					.isDefined
		impl(desired)
	.size

println(s"part 1: $part1")
