//> using scala 3.3.1
//> using dep org.typelevel::toolkit:0.1.19

import scala.annotation.tailrec
import cats.syntax.all._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import fs2.io.file.Files
import fs2.io.file.Path

opaque type NodeLabel = String
object NodeLabel:
	inline def apply(s:String):NodeLabel = s

	extension (self: NodeLabel)
		// assumes all NodeLabels are three-characters long
		def isStartNode:Boolean = 'A' == self.charAt(2)
		def isEndNode:Boolean = 'Z' == self.charAt(2)

enum Direction:
	case Left
	case Right

object Direction:
	def fromChar(c: Char): Direction =
		c match
			case 'L' => Left
			case 'R' => Right

final case class Node(left: NodeLabel, right: NodeLabel):
	def apply(d: Direction): NodeLabel =
		d match
			case Direction.Left => left
			case Direction.Right => right

final case class CycleInformation(start: Int, length: Int, isEndNode: Seq[Int])

final case class Input(instructions: Seq[Direction], nodes: Map[NodeLabel, Node]):
	def findCycle(start: NodeLabel): CycleInformation =
		import scala.collection.mutable
		val seen = mutable.HashSet.empty[NodeLabel]
		val instructionCycles = mutable.Buffer.empty[(NodeLabel, Vector[Boolean])]
		var currentPosition = start

		while ! seen.contains(currentPosition) do
			seen += currentPosition

			val currentInstructionCycle = instructions.foldLeft((currentPosition, Vector.empty[Boolean])): (folding, direction) =>
				val (currentPosition, isEndNodes) = folding
				val nextPosition = nodes(currentPosition).apply(direction)
				val nextIsEndNodes = isEndNodes :+ nextPosition.isEndNode
				((nextPosition, nextIsEndNodes))

			instructionCycles += ((currentPosition, currentInstructionCycle._2))
			currentPosition = currentInstructionCycle._1

		val startOfCycle = instructionCycles.indexWhere(_._1 == currentPosition)
		val cycleEndNodes2 = instructionCycles.toVector.drop(startOfCycle).flatMap(_._2)
		val cycleEndNodes = cycleEndNodes2.zipWithIndex.collect({case (true, index) => index})
		CycleInformation(instructions.length * startOfCycle, cycleEndNodes2.length, cycleEndNodes)

	def lengthOfPath: Long =
		val startNodes = nodes.keySet.filter(_.isStartNode).toSeq
		val cycles = startNodes.map(this.findCycle)
		System.out.println(cycles)

		// conveniently, all cycles start at 0, reach only one end node, and reach that end node at the end of the cycle
		if cycles.exists(_.start != cycles.head.start) then
			throw new Exception()
		if cycles.exists: cycle =>
			cycle.isEndNode.length != 1 || cycle.isEndNode(0) + cycle.start == cycle.length
			then
				throw new Exception()

		@tailrec def gcd(a: Long, b: Long): Long =
			if (a < 0 || b < 0) then
				throw new IllegalArgumentException(s"gcd($a, $b)")
			end if

			if a == b then
				a
			else if a < b then
				gcd(a, b - a)
			else
				gcd(a - b, b)

		def lcm(a: Long, b: Long): Long = a * (b / gcd(a, b))

		cycles.map(_.length).map(_.toLong).foldLeft(1L)(lcm)


object Day8Part2 extends IOApp:
	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8Lines(Path("input.txt"))
			.filter:
				_.nonEmpty
			.compile.toList
			.map: lines =>
				val instructions = lines.head.map(Direction.fromChar)
				val network = lines.tail
					.map:
						case s"$from = ($left, $right)" =>
							((NodeLabel(from), Node(NodeLabel(left), NodeLabel(right))))
					.toMap
				Input(instructions, network)
			.map:
				_.lengthOfPath
			.flatMap:
				IO.println
			.map:
				_ => ExitCode(0)
