//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.annotation.tailrec

extension (self: Tuple2[Seq[StringGrid], Seq[String]])
	def finish: Seq[StringGrid] =
		new StringGrid(self._2.reverse) +: self._1

extension (self: String)
	def differences(other: String): Int =
		self.zip(other).count:
			(a, b) => a != b

class StringGrid(backing: Seq[String]):
	override def toString:String = backing.mkString("\n")

	def transpose: StringGrid = new StringGrid(backing.transpose.map(_.mkString))

	/** returns -1 if none exists */
	def findHorizontalReflection: Int =
		val zeroIndexed = (0 until backing.length - 1)
			.map: index =>
				if index < backing.length / 2 then
					(0 to index).map: j =>
						backing(index - j) differences backing(index + 1 + j)
					.sum
				else
					(0 until backing.length - 1 - index).map: j =>
						backing(index - j) differences backing(index + 1 + j)
					.sum
			.indexWhere:
				_ == 1

		if zeroIndexed < 0 then
			zeroIndexed
		else
			zeroIndexed + 1

object Day13Part2:
	def main(args:Array[String]):Unit =
		val input =
			os.read.lines(os.pwd / "input.txt")
				.foldLeft((Seq.empty[StringGrid], Seq.empty[String])): (folding, line) =>
					val (complete, current) = folding
					if line.isEmpty then
						(new StringGrid(current.reverse) +: complete, Seq.empty[String])
					else
						(complete, line +: current)
				.finish
				.reverse

		val horizontalReflections =
			input
				.map:
					_.findHorizontalReflection
				.filter:
					_ >= 0

		val verticalReflections =
			input
				.map:
					_.transpose
				.map:
					_.findHorizontalReflection
				.filter:
					_ >= 0

		val score = 100 * horizontalReflections.sum + verticalReflections.sum

		System.out.print("part 2: ")
		System.out.println:
			score
