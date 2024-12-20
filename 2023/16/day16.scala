//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2
//> using file ../Grid.scala

import name.rayrobdod.aoc.*

enum Part:
	case Empty
	case Mirror, BackMirror
	case NSSplitter, EWSplitter

	def directionChange(in: Direction): Set[Direction] =
		import Direction.*
		this match
			case Empty =>
				Set(in)
			case Mirror =>
				Set:
					in match
						case Up => Right
						case Left => Down
						case Down => Left
						case Right => Up
			case BackMirror =>
				Set:
					in match
						case Up => Left
						case Left => Up
						case Down => Right
						case Right => Down
			case NSSplitter =>
				in match
					case Up | Down => Set(in)
					case Left | Right => Set(Up, Down)
			case EWSplitter =>
				in match
					case Up | Down => Set(Left, Right)
					case Left | Right => Set(in)
	end directionChange
end Part

object Part:
	def fromChar(c: Char): Part =
		c match
			case '.' => Empty
			case '/' => Mirror
			case '\\' => BackMirror
			case '|' => NSSplitter
			case '-' => EWSplitter
end Part

def energized(initial: (Point, Direction), contraption: Grid[Part]): Int =
	val (_, seen) = contraption.explore[Direction, Direction, Unit](
		initial,
		(_, _) => 0,
		d => (d, ()),
		(_, _) => true,
		(point, direction) => contraption(point).directionChange(direction),
		(_, _, _, direction, _) => direction,
	)

	seen
		.keySet
		.map(_._1)
		// still a set, so is still distinct
		.size
end energized

def possibleStarts(contraption: Grid[_]): Seq[(Point, Direction)] =
	val xs = (0 until contraption.width).flatMap: x =>
		Seq(
			(Point(x, 0), Direction.Down),
			(Point(x, contraption.height - 1), Direction.Up)
		)

	val ys = (0 until contraption.height).flatMap: y =>
		Seq(
			(Point(0, y), Direction.Right),
			(Point(contraption.width - 1, y), Direction.Left)
		)

	xs ++ ys
end possibleStarts

object Day16:
	def main(args:Array[String]):Unit =
		val input =
			Grid.fromStrings:
				os.read.lines(os.pwd / "input.txt")
			.map:
				Part.fromChar

		System.out.print("part 1: ")
		System.out.println:
			val initial = (Point(0, 0), Direction.Right)
			energized(initial, input)

		System.out.print("part 2: ")
		System.out.println:
			possibleStarts(input)
				.map: start =>
					energized(start, input)
				.max
