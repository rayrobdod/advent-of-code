//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

import scala.collection.immutable.ArraySeq

object Day11Part1:
	def main(args: Array[String]): Unit =
		val input: Map[String, Seq[String]] =
			os.read.lines(os.pwd / "input.txt")
				.map: line =>
					val a = line.split(':')
					val b = a(1).strip.split(' ')
					a(0) -> ArraySeq.unsafeWrapArray(b)
				.toMap

		val result = 	Seq
			.unfold[Int, Seq[String]](Seq("you")): folding =>
				if folding.isEmpty then
					None
				else
					val next = folding.flatMap(input)
					val (outs, notouts) = next.partition(_ == "out")
					Some((outs.size, notouts))
			.sum

		System.out.println(s"part 1: ${result}")
