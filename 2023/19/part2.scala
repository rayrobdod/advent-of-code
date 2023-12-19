//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.collection.immutable.Queue

enum Category:
	case X, M, A, S
end Category

object Category:
	def fromChar(c: String): Category =
		c match
			case "x" => Category.X
			case "m" => Category.M
			case "a" => Category.A
			case "s" => Category.S
end Category

enum Comparison:
	case <, >
end Comparison

type WorkflowName = String
val Rejected:WorkflowName = "R"
val Accepted:WorkflowName = "A"

case class Workflow(rules: Seq[Rule], orElse: WorkflowName):
	extension (self:(List[(PartSet, WorkflowName)], PartSet))
		def finish: List[(PartSet, WorkflowName)] = ((self._2, orElse)) :: self._1

	def apply(parts: PartSet): Seq[(PartSet, WorkflowName)] =
		rules
			.foldLeft((List.empty[(PartSet, WorkflowName)], parts)): (folding, rule) =>
				val (splits, restOfParts) = folding
				val (applied, nextRestOfParts) = rule.split(restOfParts)
				((((applied, rule.result)) :: splits, nextRestOfParts))
			.finish
end Workflow

case class Rule(category:Category, comparison:Comparison, value: Int, result: WorkflowName):
	def split(parts: PartSet): (PartSet, PartSet) =
		parts.split(category, comparison, value)
end Rule

case class Range(min: Int, max: Int):
	if max < min then throw new IllegalArgumentException("")

	def size = max - min + 1

	def filter(c:Comparison, value:Int): Range =
		c match
			case Comparison.< => Range(min, math.min(max, value - 1))
			case Comparison.> => Range(math.max(min, value + 1), max)

	def filterNot(c:Comparison, value:Int): Range =
		c match
			case Comparison.< => Range(math.max(min, value), max)
			case Comparison.> => Range(min, math.min(max, value))
end Range

object Range:
	def full: Range = Range(1, 4000)
end Range

case class PartSet(x:Range, m:Range, a:Range, s:Range):
	def apply(c:Category):Range =
		c match
			case Category.X => this.x
			case Category.M => this.m
			case Category.A => this.a
			case Category.S => this.s

	def updated(c: Category, newValue: Range): PartSet =
		c match
			case Category.X => this.copy(x = newValue)
			case Category.M => this.copy(m = newValue)
			case Category.A => this.copy(a = newValue)
			case Category.S => this.copy(s = newValue)

	def split(category:Category, comparison:Comparison, value: Int): (PartSet, PartSet) =
		val categoryRange = this(category)
		((
			this.updated(category, categoryRange.filter(comparison, value)),
			this.updated(category, categoryRange.filterNot(comparison, value)),
		))
	end split

	def size: Long =
		x.size.toLong * m.size * a.size * s.size
end PartSet

object PartSet:
	def full: PartSet = PartSet(Range.full, Range.full, Range.full, Range.full)
end PartSet

def run(workflows: Map[WorkflowName, Workflow]): Set[PartSet] =
	var accepted = Set.empty[PartSet]
	var futureSteps = Queue.empty[(PartSet, WorkflowName)]
	futureSteps = futureSteps.enqueue((PartSet.full, "in"))

	while futureSteps.nonEmpty do
		val ((nextStep, nextFutureSteps)) = futureSteps.dequeue
		futureSteps = nextFutureSteps

		val (nextPartSet, nextWorkflowName) = nextStep
		val nextWorkflow = workflows(nextWorkflowName)
		nextWorkflow.apply(nextPartSet).foreach:
			case (acceptedPartSet, Accepted) =>
				accepted = accepted + acceptedPartSet
			case (_, Rejected) =>
				()
			case step =>
				futureSteps = futureSteps.enqueue(step)

	end while

	accepted
end run

object Day19Part2:
	def main(args:Array[String]):Unit =
		val workflows = locally:
			val lines = os.read.lines(os.pwd / "input.txt")
			val split = lines.indexOf("")
			val workflows = lines.take(split)
				.map:
					case s"$name{$rulesAll}" =>
						val rulesSplit = rulesAll.split(',')
						val orElse = rulesSplit.last
						val rules = rulesSplit.init.map:
							case s"$category<$value:$result" =>
								Rule(Category.fromChar(category), Comparison.<, value.toInt, result)
							case s"$category>$value:$result" =>
								Rule(Category.fromChar(category), Comparison.>, value.toInt, result)
						name -> Workflow(rules.toSeq, orElse)
				.toMap
			workflows

		System.out.print("part 2: ")
		System.out.println:
			run(workflows)
				.map:
					_.size
				.sum
