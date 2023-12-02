//> using dep org.typelevel::toolkit:0.1.19
//> using dep org.typelevel::kittens:3.1.0

import cats.syntax.all._
import cats.effect.{IO, IOApp}
import fs2.{Stream, text}
import fs2.io.file.{Files, Path}
import cats.effect.ExitCode

given cats.Monoid[Int] with
	def combine(left:Int, right:Int):Int = math.max(left, right)
	def empty:Int = Int.MinValue

case class Counts(red:Int, green:Int, blue:Int):
	def hasNoMoreThan(right:Counts): Boolean =
		this.red <= right.red && this.green <= right.green && this.blue <= right.blue

given countsMaxSemigroup: cats.Monoid[Counts] = cats.derived.semiauto.monoid

object Day2Part1 extends IOApp:
	def maxWanted = Counts(red = 12, green = 13, blue = 14)

	def minimumMarblesRequired(line:String):(Int, Counts) =
		val s"Game $index: $pulls" = line: @unchecked
		val counts = pulls.split(";").toSeq
			.flatMap: pull =>
				pull.split(",").toSeq.map: item =>
					item.strip() match
						case s"$count red" => Counts(count.toInt, 0, 0)
						case s"$count green" => Counts(0, count.toInt, 0)
						case s"$count blue" => Counts(0, 0, count.toInt)
			.combineAll
		(index.toInt, counts)

	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8Lines(Path("input.txt"))
			.filter(_ != "")
			.map(minimumMarblesRequired)
			.collect[Int]({case (index, counts) if (counts.hasNoMoreThan(maxWanted)) => index})
			.compile.fold[Int](0)(_+_)
			.flatMap(IO.println)
			.map(_ => ExitCode(0))
