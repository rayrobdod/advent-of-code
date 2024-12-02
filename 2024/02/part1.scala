//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

import scala.util.chaining.scalaUtilChainingOps

enum ReportStatus:
	case None
	case Increasing
	case Decreasing
	case Unsafe

def reportIsSafe(line: Seq[Int]): Boolean =
	line
		.sliding(2)
		.map: pair =>
			pair(0) - pair(1)
		.foldLeft(ReportStatus.None): (folding, elem) =>
			import ReportStatus.*
			folding match
				case Unsafe => Unsafe
				case Increasing =>
					if 1 <= elem && elem <= 3 then Increasing
					else Unsafe
				case Decreasing =>
					if -1 >= elem && elem >= -3 then Decreasing
					else Unsafe
				case None =>
					if (1 <= elem) && (elem <= 3) then Increasing
					else if (-1 >= elem) && (elem >= -3) then Decreasing
					else Unsafe
		.pipe: elem =>
			elem != ReportStatus.Unsafe

object Day2Part1:
	def main(args: Array[String]): Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val reports: Seq[Seq[Int]] = lines.map: line =>
			line.split(" ").toIndexedSeq.map(_.toInt)

		val result = reports.count(report => reportIsSafe(report))

		System.out.println(s"part 1: ${result}")
