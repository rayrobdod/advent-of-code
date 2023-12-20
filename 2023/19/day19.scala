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

// parts (part 1)

case class Part(x:Int, m:Int, a:Int, s:Int):
	def apply(c:Category):Int =
		c match
			case Category.X => this.x
			case Category.M => this.m
			case Category.A => this.a
			case Category.S => this.s

	def sum: Int = x + m + a + s
end Part

object Part:
	def fromLine(s:String):Part =
		s match
			case s"{x=$x,m=$m,a=$a,s=$s}" =>
				Part(x.toInt, m.toInt, a.toInt, s.toInt)
end Part

// parts (part 2)

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

// rules and workflows

enum Comparison:
	case <, >

	def apply(left:Int, right:Int): Boolean =
		this match
			case < => left < right
			case > => left > right
end Comparison

case class Rule(category:Category, comparison:Comparison, value: Int, result: WorkflowName):
	def matches(part: Part): Boolean =
		comparison(part(category), value)

	def split(parts: PartSet): (PartSet, PartSet) =
		parts.split(category, comparison, value)
end Rule

type WorkflowName = WorkflowName.Type

object WorkflowName:
	opaque type Type = String

	inline def apply(s:String):WorkflowName = s

	val Rejected: WorkflowName = "R"
	val Accepted: WorkflowName = "A"
	val Initial: WorkflowName = "in"
end WorkflowName

case class Workflow(rules: Seq[Rule], orElse: WorkflowName):
	def apply(part: Part): WorkflowName =
		rules
			.find:
				_.matches(part)
			.map:
				_.result
			.getOrElse:
				orElse

	def apply(parts: PartSet): Seq[(PartSet, WorkflowName)] =
		extension (self:(List[(PartSet, WorkflowName)], PartSet))
			def finish: List[(PartSet, WorkflowName)] = ((self._2, orElse)) :: self._1

		rules
			.foldLeft((List.empty[(PartSet, WorkflowName)], parts)): (folding, rule) =>
				val (splits, restOfParts) = folding
				val (applied, nextRestOfParts) = rule.split(restOfParts)
				((((applied, rule.result)) :: splits, nextRestOfParts))
			.finish
end Workflow

object Workflow:
	def fromLine(s:String): (WorkflowName, Workflow) =
		s match
			case s"$name{$rulesAll}" =>
				val rulesSplit = rulesAll.split(',')
				val orElse = rulesSplit.last
				val rules = rulesSplit.init.map:
					case s"$category<$value:$result" =>
						Rule(Category.fromChar(category), Comparison.<, value.toInt, WorkflowName(result))
					case s"$category>$value:$result" =>
						Rule(Category.fromChar(category), Comparison.>, value.toInt, WorkflowName(result))
				WorkflowName(name) -> Workflow(rules.toSeq, WorkflowName(orElse))
end Workflow

// part 1 algorithm

def isAcceptable(workflows: Map[WorkflowName, Workflow])(part: Part): Boolean =
	import WorkflowName.*
	var current: WorkflowName = Initial
	while current != Rejected && current != Accepted do
		val workflow = workflows(current)
		current = workflow.apply(part)
	end while
	current == Accepted
end isAcceptable

// part 2 algorithm

def findAllAcceptable(workflows: Map[WorkflowName, Workflow]): Set[PartSet] =
	import WorkflowName.*
	var accepted = Set.empty[PartSet]
	var futureSteps = Queue.empty[(PartSet, WorkflowName)]
	futureSteps = futureSteps.enqueue((PartSet.full, Initial))

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
end findAllAcceptable

object Day19Part1:
	def main(args:Array[String]):Unit =
		val (workflows, parts) = locally:
			val lines = os.read.lines(os.pwd / "input.txt")
			val split = lines.indexOf("")
			val workflows = lines.take(split)
				.map:
					Workflow.fromLine
				.toMap

			val parts = lines.drop(split + 1)
				.map:
					Part.fromLine
			((workflows, parts))

		System.out.print("part 1: ")
		System.out.println:
			parts
				.filter:
					isAcceptable(workflows)
				.map:
					_.sum
				.sum

		System.out.print("part 2: ")
		System.out.println:
			findAllAcceptable(workflows)
				.map:
					_.size
				.sum
