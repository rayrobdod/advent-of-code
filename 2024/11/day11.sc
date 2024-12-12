//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep org.typelevel::cats-core:2.12.0

val input: Seq[Long] =
	os.read(os.pwd / "input.txt")
		.split(" ")
		.toSeq
		.map(_.strip.toLong)

def blinkOnce1(stone: Long): Seq[Long] =
	if 0 == stone then
		Seq(1)
	else
		val stoneAsString = stone.toString
		if stoneAsString.length % 2 == 0 then
			stoneAsString.splitAt(stoneAsString.length / 2).toList.map(_.toLong)
		else
			Seq(stone * 2024)
end blinkOnce1

locally:
	def blinkOnce(stones: Seq[Long]): Seq[Long] =
		stones.flatMap(blinkOnce1)

	def blinkNTimes(initial: Seq[Long], count: Int): Seq[Long] =
		(0 until count).foldLeft(initial): (stones, _) =>
			blinkOnce(stones)

	val part1 = blinkNTimes(input, 25)

	println(s"part 1 v1: ${part1.length}")

locally:
	object stonesAfterNBlinks1:
		private val memo = scala.collection.mutable.HashMap.empty[(Long, Int), Long]
		def apply(stone: Long, blinks: Int): Long =
			if blinks == 0 then
				1
			else if memo.isDefinedAt((stone, blinks)) then
				memo((stone, blinks))
			else
				val stoneAsString = stone.toString
				if 0 == stone then
					this(1, blinks - 1)
				else if stoneAsString.length % 2 == 0 then
					val (left, right) = stoneAsString.splitAt(stoneAsString.length / 2)
					this(left.toLong, blinks - 1) + this(right.toLong, blinks - 1)
				else
					this(stone * 2024, blinks - 1)
				end if
			end if
		end apply
	end stonesAfterNBlinks1

	def stonesAfterNBlinks(initial: Seq[Long], count: Int): Long =
		initial.map(stone => stonesAfterNBlinks1(stone, count)).sum

	println(s"part 1 v2: ${stonesAfterNBlinks(input, 25)}")

locally:
	import cats.syntax.all._
	val inputV3: Map[Long, Long] = input.groupMapReduce(k => k)(v => 1L)(_ + _)

	def blinkOnce(stones: Map[Long, Long]): Map[Long, Long] =
		stones
			.view
			.foldLeft(Map.empty[Long, Long]): (folding, stoneValueAndCount) =>
				val (value, count) = stoneValueAndCount
				val afterBlinkValueCounts = blinkOnce1(value)
					.map: afterBlinkValue =>
						afterBlinkValue -> count
					.foldLeft(Map.empty[Long, Long]): (folding, newStoneValueAndCount) =>
						folding |+| Map(newStoneValueAndCount)
				folding |+| afterBlinkValueCounts

	def blinkNTimes(initial: Map[Long, Long], blinks: Int): Map[Long, Long] =
		(0 until blinks).foldLeft(initial): (stones, _) =>
			blinkOnce(stones)

	val part1 = blinkNTimes(inputV3, 25)
	val part1s = part1.iterator.map(_._2).sum

	println(s"part 1 v3: ${part1s}")

	val part2 = blinkNTimes(part1, 75 - 25)
	val part2s = part2.iterator.map(_._2).sum

	println(s"part 2 v3: ${part2s}")
