//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep org.scala-lang.modules::scala-collection-contrib:0.4.0

import scala.collection.immutable.MultiDict

val input: Seq[String] = os.read.lines(os.pwd / "input.txt")

val rules = input
	.takeWhile(_ != "")
	.map({ case s"${a}|${b}" => (a, b) })
	.to(MultiDict)

val manuals = input
	.dropWhile(_ != "")
	.drop(1)
	.map(_.split(",").toSeq)

def isManualValid(manual: Seq[String]): Boolean =
	case class State(valid: Boolean, seen: Set[String])
	object State:
		def empty: State = State(true, Set.empty)

	manual
		.foldLeft(State.empty): (folding, elem) =>
			val shouldNotBeSeen = rules.get(elem)
			val thisPageIsValid = shouldNotBeSeen.intersect(folding.seen).isEmpty
			State(folding.valid & thisPageIsValid, folding.seen + elem)
		.valid

def middleValue(manual: Seq[String]): Int =
	manual(manual.length / 2).toInt

locally:
	val validManuals = manuals.filter(isManualValid)
	val part1 = validManuals.map(middleValue).sum

	System.out.println(s"part 1: ${part1}")

locally:
	val invalidManuals = manuals.filterNot(isManualValid)

	object RulesOrdering extends Ordering[String]:
		def compare(x: String, y: String): Int =
			if (rules.get(x).contains(y)) then
				1
			else if (rules.get(y).contains(x)) then
				-1
			else
				throw new Exception(s"$x, $y")

	val fixedManuals = invalidManuals.map: manual =>
		manual.sorted(using RulesOrdering)

	val part2 = fixedManuals.map(middleValue).sum

	System.out.println(s"part 2: ${part2}")
