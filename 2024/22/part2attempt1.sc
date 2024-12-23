//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

val inputFile = os.pwd / "input.txt"

val modulus = 16777216

val initialSecretNumbers: Seq[Int] =
	os.read.lines(inputFile).map(_.toInt)

def next(in: Int): Int =
	val a = ((in * 64) ^ in) % modulus
	val b = ((a / 32) ^ a) % modulus
	val c = ((b * 2048L) ^ b) % modulus
	c.toInt

val part1 = initialSecretNumbers
	.map: initialSecretNumber =>
		(0 until 2000).foldLeft(initialSecretNumber): (folding, _) =>
			next(folding)
	.map:
		_.toLong
	.sum

println(s"part 1: $part1")

/*
val priceSequences = Seq(123)
	.map: initialSecretNumber =>
		Seq
			.iterate(initialSecretNumber, 10)(next)
			.map(_ % 10)
end priceSequences
*/

val priceSequences = initialSecretNumbers
	.map: initialSecretNumber =>
		scala.collection.immutable.ArraySeq
			.iterate(initialSecretNumber, 2000)(next)
			.map(_ % 10)
end priceSequences

val buyerSequences: Iterable[(Int, Int, Int, Int)] = new:
	def iterator: Iterator[(Int, Int, Int, Int)] = new:
		private var _1 = -9
		private var _2 = -9
		private var _3 = -9
		private var _4 = -10

		def hasNext: Boolean =
			_1 != 9 || _2 != 9 || _3 != 9 || _4 != 9

		def next(): (Int, Int, Int, Int) =
			_4 += 1
			if _4 > 9 then
				_3 += 1
				_4 = -9
			if _3 > 9 then
				_2 += 1
				_3 = -9
			if _2 > 9 then
				_1 += 1
				_2 = -9
				println(_1)
			(_1, _2, _3, _4)
end buyerSequences

def priceIfDeltasMatch(trigger: Seq[Int]): PartialFunction[Seq[Int], Int] = new:
	def apply(segment: Seq[Int]): Int =
		println(segment)
		segment.last
	def isDefinedAt(segment: Seq[Int]): Boolean =
		val deltas = segment
			.sliding(2, 1)
			.map: ab =>
				val Seq(a, b) = ab
				b - a
			.toSeq
		deltas == trigger
end priceIfDeltasMatch

/*
def part2v1 = buyerSequences
	.map[Int]: buyerSequence =>
		priceSequences
			.view
			.map: priceSequence =>
				priceSequence
					.sliding(5, 1)
					.collectFirst:
						priceIfDeltasMatch(buyerSequence.toList)
					.getOrElse(0)
			.sum
	.max
end part2v1

println(s"part 2: $part2")
*/

extension (self: Seq[Int])
	def priceAtDeltas(trigger: (Int, Int, Int, Int)): Int =
		var _1: Int = -100
		var _2: Int = -100
		var _3: Int = -100
		var _4: Int = -100
		var prev: Int = -100
		val it = self.iterator
		while (it.hasNext)
			_1 = _2
			_2 = _3
			_3 = _4
			val current: Int = it.next()
			_4 = current - prev
			prev = current

			if _1 == trigger._1
				&& _2 == trigger._2
				&& _3 == trigger._3
				&& _4 == trigger._4
			then
				return current
		end while
		return 0
	end priceAtDeltas

def part2 = buyerSequences
	.view
	.map[Int]: buyerSequence =>
		priceSequences
			.view
			.map: priceSequence =>
				priceSequence
					.priceAtDeltas(buyerSequence)
			.sum
	.max
end part2

println(s"part 2: $part2")
