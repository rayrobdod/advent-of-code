//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

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

println(s"part 1 v1: ${part1.length}")

object stonesAfterNBlinks:
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
end stonesAfterNBlinks

def stonesAfterNBlinks(initial: Seq[Long], count: Int): Long =
	initial.map(stone => stonesAfterNBlinks(stone, count)).sum

println(s"part 1 v2: ${stonesAfterNBlinks(input, 25)}")
