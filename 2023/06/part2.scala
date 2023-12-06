//> using scala 3.3.1
//> using dep org.typelevel::toolkit:0.1.19

import cats.syntax.all._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.parse.Parser
import fs2.io.file.Files
import fs2.io.file.Path

final case class Race(totalTime:Int, record:Long):
	def waysToBeatRecord:Int =
		(0 to totalTime).count: holdTime =>
			val runTime = totalTime - holdTime
			val distance = holdTime.toLong * runTime
			distance > record

object Day6Part2 extends IOApp:
	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8Lines(Path("input.txt"))
			.map(_.split(":").last.replaceAll(" ", ""))
			.compile.toList
			.map: (raceParts: List[String]) =>
				val time :: distance :: Nil = raceParts: @unchecked
				Race(time.toInt, distance.toLong)
			.map: race =>
				race.waysToBeatRecord
			.flatMap(IO.println)
			.map(_ => ExitCode(0))
