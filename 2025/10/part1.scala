//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

import scala.collection.immutable.BitSet

case class Machine(target:BitSet, buttons:Seq[BitSet]):
	def minButtonPressesToTurnOn =
		(0 until (1 << buttons.size))
			.map: buttonPressesInt =>
				BitSet.fromBitMask(Array(buttonPressesInt))
			.filter: buttonPresses =>
				val result = buttonPresses
					.toSeq
					.map: index =>
						buttons(index)
					.foldLeft(BitSet.empty): (folding, button) =>
						folding.xor(button)
				result == target
			.map: buttonPresses =>
				buttonPresses.size
			.min
end Machine

object Machine:
	def fromLine(line: String): Machine =
		val targetStr = line.substring(line.indexOf("[") + 1, line.indexOf("]"))
		val target = targetStr
			.zipWithIndex
			.filter: (c, index) =>
				c == '#'
			.map:
				_._2
			.to(BitSet)

		var buttonsStrs: Seq[String] = Vector.empty
		var left: Int = line.indexOf('(')
		while (left != -1)
			val right: Int = line.indexOf(')', left)
			buttonsStrs = buttonsStrs :+ line.substring(left + 1, right)
			left = line.indexOf('(', right)

		val buttons = buttonsStrs
			.map: s =>
				s.split(',').map(_.toInt).to(BitSet)

		Machine(target, buttons)
end Machine

object Day10Part1:
	def main(args: Array[String]): Unit =
		val input: Seq[Machine] =
			os.read.lines(os.pwd / "input.txt").map(Machine.fromLine)

		val result = input.map(_.minButtonPressesToTurnOn).sum

		System.out.println(s"part 1: ${result}")
