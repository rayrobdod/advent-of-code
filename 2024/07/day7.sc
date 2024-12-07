//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

case class Equation(result: Long, arguments: Seq[Long]):
	def couldBeTrue(ops: Seq[(Long, Long) => Long]): Boolean =
		arguments.tail
			.foldLeft[Set[Long]](Set(arguments.head)): (possibleResults, arg) =>
				for
					possibleResult <- possibleResults;
					op <- ops;
					newPossibleResult = op(possibleResult, arg)
					// all operations have results larger than the two arguments.
					// if the resultSoFar is larger than the expected result, that cannot be the correct operator set
					// This filter seems to halve execution time
					// measured without filter: ~14s. Measured with filter ~7s
					if newPossibleResult <= result
				yield
					newPossibleResult
				end for
			.contains(result)

val input_raw: Seq[String] = os.read.lines(os.pwd / "input.txt")
val input: Seq[Equation] = input_raw.map:
	case s"$r: $args" => Equation(r.toLong, args.split(' ').toSeq.map(_.toLong))

val part1 = input
	.filter(_.couldBeTrue(Seq(_ + _, _ * _)))
	.map(_.result)
	.sum

println(s"part 1: ${part1}")

val part2 = input
	.filter(_.couldBeTrue(Seq(_ + _, _ * _, { (a, b) => s"$a$b".toLong })))
	.map(_.result)
	.sum

println(s"part 2: ${part2}")
