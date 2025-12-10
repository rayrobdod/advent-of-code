//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

import scala.collection.immutable.ArraySeq

extension (x: Seq[Int])
	def |*|(factor: Int): Seq[Int] = x.map(_ * factor)
	def |+|(y: Seq[Int]): Seq[Int] = x.zip(y).map(_+_)
	def |>|(y: Seq[Int]): Boolean = x.zip(y).exists(_>_)
	def updatedWith(index: Int, fn: Int => Int): Seq[Int] = x.updated(index, fn(x(index)))

case class Machine(targetJoltage:Seq[Int], buttons:Seq[Seq[Int]]):
	private val zeroJoltage: Seq[Int] = targetJoltage.map(_ => 0)

	private def rec(buttonPresses: Seq[Int], mostRecentButtonPress: Int): Int =
		val currentJoltage = buttonPresses
			.zipWithIndex
			.map: (count, index) =>
				buttons(index) |*| count
			.foldLeft(zeroJoltage): (folding, addend) =>
				folding |+| addend

		if currentJoltage == targetJoltage then
			buttonPresses.sum
		else if currentJoltage |>| targetJoltage then
			0xFFFFFF
		else
			(mostRecentButtonPress until buttonPresses.size)
				.map: index =>
					rec(buttonPresses.updatedWith(index, _ + 1), index)
				.min

	def minButtonPressesToTurnOn =
		val buttonPresses = buttons.map(_ => 0)
		val retval = rec(buttonPresses, 0)
		retval
end Machine

object Machine:
	def fromLine(line: String): Machine =
		val joltageStr = line.substring(line.indexOf("{") + 1, line.indexOf("}"))
		val joltage = joltageStr.split(',').map(_.toInt).to(ArraySeq)

		var buttonsStrs: Seq[String] = Vector.empty
		var left: Int = line.indexOf('(')
		while (left != -1)
			val right: Int = line.indexOf(')', left)
			buttonsStrs = buttonsStrs :+ line.substring(left + 1, right)
			left = line.indexOf('(', right)

		val buttons = buttonsStrs
			.map: s =>
				s.split(',').map(_.toInt).foldLeft(joltage.map(_ => 0)): (folding, index) =>
					folding.updated(index, 1)

		Machine(joltage, buttons)
end Machine

object Day10Part1:
	def main(args: Array[String]): Unit =
		val input: Seq[Machine] =
			os.read.lines(os.pwd / "sample.txt").map(Machine.fromLine)

		val result = input.map(_.minButtonPressesToTurnOn).sum

		System.out.println(s"part 2: ${result}")
