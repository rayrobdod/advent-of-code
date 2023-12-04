//> using scala 3.3.1
//> using dep org.typelevel::toolkit:0.1.19

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import cats.syntax.all._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import fs2.io.file.Files
import fs2.io.file.Path

case class Card(winningNumbers: BitSet, havingNumbers: BitSet):
	def matches:Int = (winningNumbers & havingNumbers).size
	def points:Int = if matches == 0 then 0 else 1 << (matches - 1)

object Card:
	def fromLine(line:String):Card =
		val numbers = line.split(':').apply(1)
		val winners = numbers.split('|').apply(0)
		val havers = numbers.split('|').apply(1)
		val winners2 = winners.split(' ').filter(_ != "").map(_.toInt).to(BitSet)
		val havers2 = havers.split(' ').filter(_ != "").map(_.toInt).to(BitSet)
		Card(winners2, havers2)

object Day4Part1 extends IOApp:
	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8Lines(Path("input.txt"))
			.filter(_ != "")
			.map(Card.fromLine)
			.map(_.points)
			.compile.foldMonoid
			.flatMap(IO.println)
			.map(_ => ExitCode(0))

object Day4Part2 extends IOApp:
	def cardCount(originalCardList:List[Card]):Int =
		@tailrec def rec(cardList:List[(Card, Int)], previousCardCounts:Int):Int =
			cardList match
				case Nil => previousCardCounts
				case ((currentCard, currentCardCount)) :: rest =>
					val cardsToDuplicate = currentCard.matches
					rec(
						rest.take(cardsToDuplicate).map(x => ((x._1, x._2 + currentCardCount))) ::: rest.drop(cardsToDuplicate),
						previousCardCounts + currentCardCount
					)
		rec(originalCardList.map(c => ((c, 1))), 0)

	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8Lines(Path("input.txt"))
			.filter(_ != "")
			.map(Card.fromLine)
			.compile.toList
			.map(cardCount)
			.flatMap(IO.println)
			.map(_ => ExitCode(0))
