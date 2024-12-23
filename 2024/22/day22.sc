//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

val inputFile = os.pwd / "input.txt"

val initialSecretNumbers: Seq[Int] =
	os.read.lines(inputFile).map(_.toInt)

def next(in: Int): Int =
	/*
	val modulus = 16777216
	val a = ((in * 64) ^ in) % modulus
	val b = ((a / 32) ^ a) % modulus
	val c = ((b * 2048L) ^ b) % modulus
	c.toInt
	*/
	val a = ((in << 6) ^ in) & 0xFFFFFF
	val b = ((a >> 5) ^ a) & 0xFFFFFF
	val c = ((b << 11) ^ b) & 0xFFFFFF
	c
end next

val secretSequences =
	initialSecretNumbers
		.map: initialSecretNumber =>
			scala.collection.immutable.ArraySeq
				.iterate(initialSecretNumber, 2001)(next)

val part1 = secretSequences
	.map:
		_.last.toLong
	.sum

println(s"part 1: $part1")

val priceSequences =
	secretSequences.map:
		_.map:
			_ % 10

final class MutPricesAtTrigger:
	private val backing = Array.fill[Int](19, 19, 19, 19)(-1)

	def isSetAt(n1: Int, n2: Int, n3: Int, n4: Int): Boolean =
		backing(n1 + 9)(n2 + 9)(n3 + 9)(n4 + 9) >= 0

	def get(n1: Int, n2: Int, n3: Int, n4: Int): Int =
		if n1 < -9 || 9 < n1 then throw new IndexOutOfBoundsException(s"$n1, $n2, $n3, $n4")
		if n2 < -9 || 9 < n2 then throw new IndexOutOfBoundsException(s"$n1, $n2, $n3, $n4")
		if n3 < -9 || 9 < n3 then throw new IndexOutOfBoundsException(s"$n1, $n2, $n3, $n4")
		if n4 < -9 || 9 < n4 then throw new IndexOutOfBoundsException(s"$n1, $n2, $n3, $n4")
		backing(n1 + 9)(n2 + 9)(n3 + 9)(n4 + 9)
	end get

	def update(n1: Int, n2: Int, n3: Int, n4: Int, newValue: Int): Unit =
		backing(n1 + 9)(n2 + 9)(n3 + 9)(n4 + 9) = newValue
	end update

	def setRestToZero(): Unit =
		for
			n1 <- 0 until backing.length
			n2 <- 0 until backing(n1).length
			n3 <- 0 until backing(n1)(n2).length
			n4 <- 0 until backing(n1)(n2)(n3).length
			if -1 == backing(n1)(n2)(n3)(n4)
		do
			backing(n1)(n2)(n3)(n4) = 0
	end setRestToZero

	def |+|(other: MutPricesAtTrigger): MutPricesAtTrigger =
		val retval = new MutPricesAtTrigger()
		for
			n1 <- 0 until backing.length
			n2 <- 0 until backing(n1).length
			n3 <- 0 until backing(n1)(n2).length
			n4 <- 0 until backing(n1)(n2)(n3).length
		do
			retval.backing(n1)(n2)(n3)(n4) =
				this.backing(n1)(n2)(n3)(n4) +
					other.backing(n1)(n2)(n3)(n4)
		end for
		retval
	end |+|

	def max: Int =
		backing
			.map:
				_
					.map:
						_
							.map:
								_.max
							.max
					.max
			.max
	end max
end MutPricesAtTrigger

object MutPricesAtTrigger:
	def zero: MutPricesAtTrigger =
		val retval = new MutPricesAtTrigger()
		retval.setRestToZero()
		retval
	end zero
end MutPricesAtTrigger

val pricesAtTrigger =
	priceSequences
		.map: priceSequence =>
			val retval = new MutPricesAtTrigger()
			val it = priceSequence.iterator
			var _1: Int = it.next()
			var _2: Int = it.next() - _1
			var _3: Int = it.next() - _2 - _1
			var prev: Int = it.next()
			var _4: Int = prev - _3 - _2 - _1

			while (it.hasNext)
				_1 = _2
				_2 = _3
				_3 = _4
				val current: Int = it.next()
				_4 = current - prev
				prev = current

				//println(s"$_1, $_2, $_3, $_4 -> $current")

				if ! retval.isSetAt(_1, _2, _3, _4) then
					retval.update(_1, _2, _3, _4, current)
			end while
			retval.setRestToZero()
			retval
		.foldLeft(MutPricesAtTrigger.zero):
			_ |+| _

println(s"part 2: ${pricesAtTrigger.max}")
