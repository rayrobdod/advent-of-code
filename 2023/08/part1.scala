//> using scala 3.3.1
//> using dep org.typelevel::toolkit:0.1.19

import scala.annotation.tailrec
import cats.syntax.all._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import fs2.io.file.Files
import fs2.io.file.Path

final class CyclingIterator[A] private (values: Seq[A], position: Int):
	def this(values: Seq[A]) = this(values, 0)

	def next: (A, CyclingIterator[A]) =
		val nextPos1 = position + 1
		val nextPos = if nextPos1 >= values.length then 0 else nextPos1
		(values(position), new CyclingIterator(values, nextPos))


opaque type NodeLabel = String
object NodeLabel:
	inline def apply(s:String):NodeLabel = s

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

final case class Input(instructions: Seq[Direction], nodes: Map[NodeLabel, Node]):
	def lengthOfPath: Int =
		@tailrec def recurse(position: NodeLabel, instructions: CyclingIterator[Direction], count: Int): Int =
			val (nextDirection, nextInstructions) = instructions.next
			val nextPosition = nodes(position).apply(nextDirection)
			val nextCount = count + 1
			if NodeLabel("ZZZ") == nextPosition then
				nextCount
			else
				recurse(nextPosition, nextInstructions, nextCount)

		recurse(NodeLabel("AAA"), new CyclingIterator(instructions), 0)


object Day8Part1 extends IOApp:
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
			.map: input =>
				input.lengthOfPath
			.flatMap:
				IO.println
			.map:
				_ => ExitCode(0)
