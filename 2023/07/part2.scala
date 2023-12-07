//> using scala 3.3.1
//> using dep org.typelevel::toolkit:0.1.19

import cats.syntax.all._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import fs2.io.file.Files
import fs2.io.file.Path

enum Card extends java.lang.Enum[Card]:
	case Ace, King, Queen
	case Ten, Nine, Eight, Seven, Six
	case Five, Four, Three, Two
	case Joker

object Card:
	def fromChar(c:Char):Card =
		c match
			case '2' => Card.Two
			case '3' => Card.Three
			case '4' => Card.Four
			case '5' => Card.Five
			case '6' => Card.Six
			case '7' => Card.Seven
			case '8' => Card.Eight
			case '9' => Card.Nine
			case 'T' => Card.Ten
			case 'J' => Card.Joker
			case 'Q' => Card.Queen
			case 'K' => Card.King
			case 'A' => Card.Ace

enum HandType extends java.lang.Enum[HandType]:
	case FiveKind, FourKind, FullHouse, ThreeKind
	case TwoPair, OnePair, HighCard

case class Hand(cards:List[Card], bid: Int):
	def typ: HandType =
		val rawCounts = cards
			.groupBy(Predef.identity)
			.view
			.mapValues(_.length)
			.toMap

		val jokerCount = rawCounts.get(Card.Joker).getOrElse(0)

		val counts =
			(rawCounts - Card.Joker)
				.values
				.toList
				.sorted
				.reverse match
					case Nil => 5 :: Nil
					case head :: tail => (head + jokerCount) :: tail

		counts: @unchecked match
			case 5 :: Nil => HandType.FiveKind
			case 4 :: 1 :: Nil => HandType.FourKind
			case 3 :: 2 :: Nil => HandType.FullHouse
			case 3 :: 1 :: 1 :: Nil => HandType.ThreeKind
			case 2 :: 2 :: 1 :: Nil => HandType.TwoPair
			case 2 :: 1 :: 1 :: 1 :: Nil => HandType.OnePair
			case 1 :: 1 :: 1 :: 1 :: 1 :: Nil => HandType.HighCard

object Hand:
	given cats.Show[Hand] = cats.Show.fromToString

	private given Ordering[List[Card]] with
		def compare(x: List[Card], y: List[Card]): Int =
			x.zip(y)
				.map: (a, b) =>
					implicitly[Ordering[Card]].compare(a, b)
				.dropWhile: value =>
					value == 0
				.headOption
				.getOrElse(0)

	given Ordering[Hand] =
		Ordering.by[Hand, HandType](_.typ).orElseBy(_.cards)

object Day7Part2 extends IOApp:
	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8Lines(Path("input.txt"))
			.filter(_.nonEmpty)
			.map(_.split("\\s+"))
			.map({case Array(cards, bid) => Hand(cards.map(Card.fromChar).toList, bid.toInt)})
			.compile.toList
			.map: hands =>
				hands
					.sorted
					.reverse
					.zipWithIndex
					.map: (hand, index) =>
						hand.bid * (1 + index)
					.sum
					//.mkString("\n")
			.flatMap(IO.println)
			.map(_ => ExitCode(0))
