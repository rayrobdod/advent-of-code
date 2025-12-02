//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

import scala.collection.immutable.ArraySeq

object Day2Part2:
	def main(args: Array[String]): Unit =
		val txt: String = os.read(os.pwd / "input.txt").strip

		val result = ArraySeq.unsafeWrapArray(txt.split(','))
			.map: text =>
				val textSplit = text.split('-')
				(textSplit(0).toLong to textSplit(1).toLong)
			.flatMap: range =>
				range.filter: i =>
					val x = i.toString
					(1 to x.size / 2)
						.filter(factor => x.size % factor == 0)
						.exists(factor => x.substring(0, factor) * (x.size / factor) == x)
			.sum

		System.out.println(s"part 2: ${result}")
