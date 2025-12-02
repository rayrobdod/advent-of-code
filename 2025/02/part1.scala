//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

import scala.collection.immutable.ArraySeq

// Forgot to do this at the initial starting time,
// so might as well be clever:
// and try to narrow down the ranges and not check every entry

object Day2Part1:
	def main(args: Array[String]): Unit =
		val txt: String = os.read(os.pwd / "input.txt").strip

		val result = ArraySeq.unsafeWrapArray(txt.split(','))
			.map: text =>
				val textSplit = text.split('-')
				(textSplit(0), textSplit(1))
			// make all ranges start and end with same number of digits
			.flatMap: (a, b) =>
				(b.size - a.size) match
					case 0 => Seq((a, b))
					case 1 =>
						Seq((a, "9"*a.size), ("1" + "0"*a.size, b))
			// strings with odd number of digits cannot match pattern
			.filter: (a, b) =>
				a.size % 2 == 0
			//
			.flatMap: (a, b) =>
				val halfSize = a.size / 2
				val aPrefix = a.substring(0, halfSize)
				val bPrefix = b.substring(0, halfSize)
				val aSuffix = a.substring(halfSize)
				val bSuffix = b.substring(halfSize)
				val nineSuffix = "9" * halfSize
				val zeroSuffix = "0" * halfSize

				if (aPrefix == bPrefix)
					Seq((aPrefix, aSuffix, bSuffix))
				else
					(aPrefix, aSuffix, nineSuffix) +:
						((aPrefix.toInt + 1) to (bPrefix.toInt - 1)).map{x => (x.toString, zeroSuffix, nineSuffix)} :+
						(bPrefix, zeroSuffix, bSuffix)
			.filter: (prefix, suffixLeft, suffixRight) =>
				suffixLeft.toInt <= prefix.toInt && prefix.toInt <= suffixRight.toInt
			.map: (prefix, _, _) =>
				s"$prefix$prefix".toLong
			.sum

		System.out.println(s"part 1: ${result}")
