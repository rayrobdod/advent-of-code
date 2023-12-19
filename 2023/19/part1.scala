//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

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

	def apply(left:Int, right:Int): Boolean =
		this match
			case < => left < right
			case > => left > right
end Comparison

type WorkflowName = String
val Rejected:WorkflowName = "R"
val Accepted:WorkflowName = "A"

case class Workflow(rules: Seq[Rule], orElse: WorkflowName):
	def apply(part: Part): WorkflowName =
		rules
			.find:
				_.matches(part)
			.map:
				_.result
			.getOrElse:
				orElse
end Workflow

case class Rule(category:Category, comparison:Comparison, value: Int, result: WorkflowName):
	def matches(part: Part): Boolean =
		comparison(part(category), value)
end Rule

case class Part(x:Int, m:Int, a:Int, s:Int):
	def apply(c:Category):Int =
		c match
			case Category.X => this.x
			case Category.M => this.m
			case Category.A => this.a
			case Category.S => this.s

	def sum: Int = x + m + a + s
end Part

def isPartAccepted(workflows: Map[WorkflowName, Workflow])(part: Part): Boolean =
	var current: WorkflowName = "in"
	while current != Rejected && current != Accepted do
		val workflow = workflows(current)
		current = workflow.apply(part)
	end while
	current == Accepted
end isPartAccepted

object Day19Part1:
	def main(args:Array[String]):Unit =
		val (workflows, parts) = locally:
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

			val parts = lines.drop(split + 1)
				.map:
					case s"{x=$x,m=$m,a=$a,s=$s}" =>
						Part(x.toInt, m.toInt, a.toInt, s.toInt)
			((workflows, parts))

		System.out.print("part 1: ")
		System.out.println:
			parts
				.filter:
					isPartAccepted(workflows)
				.map:
					_.sum
				.sum
