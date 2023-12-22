//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.collection.immutable.Queue

case class Coord(x: Int, y: Int, z: Int)

case class Brick(end1: Coord, end2: Coord):
	if end2.x < end1.x || end2.y < end1.y || end2.z < end1.z then
		throw new IllegalArgumentException

	def moveDownBy(dz: Int): Brick =
		this.copy(end1.copy(z = end1.z - dz), end2.copy(z = end2.z - dz))

	def sharesAnyColumnWith(other: Brick): Boolean =
		def rangesOverlap(a1:Int, a2:Int, b1:Int, b2:Int): Boolean =
			(b1 <= a1 && a1 <= b2) || (b1 <= a2 && a2 <= b2) ||
				(a1 <= b1 && b1 <= a2) || (a1 <= b2 && b2 <= a2)

		rangesOverlap(this.end1.x, this.end2.x, other.end1.x, other.end2.x) &&
			rangesOverlap(this.end1.y, this.end2.y, other.end1.y, other.end2.y)
end Brick

def fall(bricks:Seq[Brick]):Seq[Brick] =
	bricks
		.sortBy: brick =>
			brick.end1.z
		.foldLeft(Seq.empty[Brick]): (pile, brick) =>
			val newZ1 = pile.filter(_.sharesAnyColumnWith(brick)).map(_.end2.z).maxOption.getOrElse(0) + 1

			pile :+ brick.moveDownBy(brick.end1.z - newZ1)
end fall

def calcSupportPairs(fallenBricks: Seq[Brick]): Seq[(Int, Int)] =
	fallenBricks.zipWithIndex.flatMap: (topBrick, topIndex) =>
		fallenBricks.zipWithIndex
			.filter: (bottomBrick, _) =>
				bottomBrick.sharesAnyColumnWith(topBrick) &&
					bottomBrick.end2.z + 1 == topBrick.end1.z
			.map: (_, bottomIndex) =>
				((bottomIndex, topIndex))
end calcSupportPairs

def countSafelyDisintegratable(numBricks: Int, supportPairs: Seq[(Int, Int)]): Int =
	val cannotBeSafelyDisintegrated: Set[Int] =
		supportPairs
			.groupMap(_._2)(_._1)
			.collect:
				case (_, Seq(x)) => x
			.toSet

	numBricks - cannotBeSafelyDisintegrated.size
end countSafelyDisintegratable

def countChainReaction(numBricks: Int, supportPairs: Seq[(Int, Int)]): Seq[Int] =
	val supporting = supportPairs.groupMap(_._1)(_._2).withDefaultValue(Seq.empty)
	val supportedBy = supportPairs.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)

	(0 until numBricks).map: firstToRemove =>
		var queue: Queue[Int] = Queue(firstToRemove)
		var destroyed: Set[Int] = Set.empty

		while (queue.nonEmpty) do
			val (toDestroy, dequeued) = queue.dequeue
			destroyed = destroyed + toDestroy

			val candidates = supporting(toDestroy)
			queue = dequeued ++ candidates.filter: candidate =>
				supportedBy(candidate).subsetOf(destroyed)
		end while

		destroyed.size - 1
end countChainReaction

object Day22:
	def main(args:Array[String]):Unit =
		val input = os.read.lines(os.pwd / "input.txt")
			.map:
				case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
					Brick(Coord(x1.toInt, y1.toInt, z1.toInt), Coord(x2.toInt, y2.toInt, z2.toInt))

		val numBricks = input.length
		val supportPairs = calcSupportPairs(fall(input))

		print("part 1: ")
		println:
			countSafelyDisintegratable(numBricks, supportPairs)

		print("part 2: ")
		println:
			countChainReaction(numBricks, supportPairs).sum
