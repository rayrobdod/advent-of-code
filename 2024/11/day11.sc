//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

import scala.collection.immutable.WrappedString

val input: Seq[Long] =
	os.read(os.pwd / "input.txt")
		.split(" ")
		.toSeq
		.map(_.strip.toLong)

def blinkOnce(stones: Seq[Long]): Seq[Long] =
	stones
		.flatMap:
			case 0 => Seq(1)
			case stone if stone.toString.length % 2 == 0 =>
				stone.toString.splitAt(stone.toString.length / 2).toList.map(_.toLong)
			case stone =>
				Seq(stone * 2024)

def blinkNTimes(initial: Seq[Long], count: Int): Seq[Long] =
	(0 until count).foldLeft(initial): (stones, _) =>
		blinkOnce(stones)

val part1 = blinkNTimes(input, 25)

println(s"part 1: ${part1.length}")

//val part2 = blinkNTimes(part1, 75 - 25)
val part2 = null

println(s"part 2: ${part2.length}")
