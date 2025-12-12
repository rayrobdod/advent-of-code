//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

import scala.collection.immutable.ArraySeq

case class Path(current: String, fft: Boolean, dac: Boolean):
	def push(part: String): Path =
		Path(part, fft || part == "fft", dac || part == "dac")
	def isEnd: Boolean = current == "out"
	def isValid: Boolean = fft && dac

object Path:
	def svr: Path = Path("svr", false, false)

object Day11Part12:
	def main(args: Array[String]): Unit =
		val input: Map[String, Seq[String]] =
			os.read.lines(os.pwd / "input.txt")
				.map: line =>
					val a = line.split(':')
					val b = a(1).strip.split(' ')
					a(0) -> ArraySeq.unsafeWrapArray(b)
				.toMap

		val result = 	Seq
			.unfold[Int, Seq[Path]](Seq(Path.svr)): paths =>
				if paths.isEmpty then
					None
				else
					val nextPaths = paths.flatMap: path =>
						input(path.current).map: next =>
							path.push(next)
					val (outs, notouts) = nextPaths.partition(_.isEnd)
					Some((outs.count(_.isValid), notouts))
			.sum

		System.out.println(s"part 1: ${result}")
