//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using file ../../2023/Grid.scala

val inputFile = os.pwd / "input.txt"

import scala.collection.mutable
import name.rayrobdod.aoc.*

object I:
	def unapply(x:String):Option[Int] = x.toIntOption

val fallingBlocks: Seq[Point] =
	os.read.lines(inputFile).map:
		case s"${I(x)},${I(y)}" => Point(x,y)

def myExplore(grid: Grid[Boolean]): Option[Int] =
	grid.explore[Int, Unit, Unit](
		start = (Point(0, 0), 0),
		priority = scala.math.Ordering.by[(Point, Int), Int](- _._2),
		toSeen = _ => ((), ()),
		continue = (p, _) => p != Point(70, 70),
		allowedNextMoves =
			case (p, _) =>
				Direction.values.toSet
					.filter: d =>
						val nextSpace = p + d.toUnitVector
						grid.isDefinedAt(nextSpace) && grid(nextSpace)
		,
		updatedState = (_, prevState, _, _, _) => prevState + 1,
	)._1.map(_._2)

val fellBlocks1k = fallingBlocks.take(1024).toSet
val grid1k = Grid.tabulate(71, 71): p =>
	! fellBlocks1k.contains(p)

println(s"part 1: ${myExplore(grid1k).get}")

val remainingFallingBlocks: mutable.Queue[Point] = mutable.Queue.from(fallingBlocks.drop(1024))
var grid = grid1k
var lastBlockToFall: Point = null

while
	lastBlockToFall = remainingFallingBlocks.dequeue
	grid = grid.updated(lastBlockToFall, false)
	myExplore(grid).isDefined
do ()

println(s"part 2: ${lastBlockToFall}")
