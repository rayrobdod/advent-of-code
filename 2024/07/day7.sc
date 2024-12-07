//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

case class Equation(result: Long, arguments: Seq[Long]):
	def couldBeTrue1: Boolean =
		arguments.tail
			.foldLeft[Set[Long]](Set(arguments.head)): (possibleResults, arg) =>
				possibleResults.flatMap: possibleResult =>
					Set(
						possibleResult + arg,
						possibleResult * arg,
					)
			.contains(result)

	def couldBeTrue2: Boolean =
		arguments.tail
			.foldLeft[Set[Long]](Set(arguments.head)): (possibleResults, arg) =>
				possibleResults.flatMap: possibleResult =>
					Set(
						possibleResult + arg,
						possibleResult * arg,
						(s"$possibleResult$arg").toLong,
					)
			.contains(result)

val input_raw: Seq[String] = os.read.lines(os.pwd / "input.txt")
val input: Seq[Equation] = input_raw.map:
	case s"$r: $args" => Equation(r.toLong, args.split(' ').toSeq.map(_.toLong))

val part1 = input
	.filter(_.couldBeTrue1)
	.map(_.result)
	.sum

System.out.println(s"part 1: ${part1}")

val part2 = input
	.filter(_.couldBeTrue2)
	.map(_.result)
	.sum

System.out.println(s"part 2: ${part2}")
