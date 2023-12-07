//> using scala 3.3.1
//> using dep org.typelevel::toolkit:0.1.19

import cats.syntax.all._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import fs2.io.file.Files
import fs2.io.file.Path

enum CardRank extends java.lang.Enum[CardRank]:
	case Ace, King, Queen
	case Ten, Nine, Eight, Seven, Six
	case Five, Four, Three, Two
	case Joker

object CardRank:
	def fromChar(c:Char):CardRank =
		c match
			case '2' => CardRank.Two
			case '3' => CardRank.Three
			case '4' => CardRank.Four
			case '5' => CardRank.Five
			case '6' => CardRank.Six
			case '7' => CardRank.Seven
			case '8' => CardRank.Eight
			case '9' => CardRank.Nine
			case 'T' => CardRank.Ten
			case 'J' => CardRank.Joker
			case 'Q' => CardRank.Queen
			case 'K' => CardRank.King
			case 'A' => CardRank.Ace

enum HandType extends java.lang.Enum[HandType]:
	case FiveKind, FourKind, FullHouse, ThreeKind
	case TwoPair, OnePair, HighCard

case class Hand(cards:List[CardRank], bid: Int):
	def typ: HandType =
		val rawCounts = cards
			.groupBy:
				Predef.identity
			.view
			.mapValues:
				_.length
			.toMap

		val jokerCount = rawCounts.get(CardRank.Joker).getOrElse(0)

		val counts =
			(rawCounts - CardRank.Joker)
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

	private given Ordering[List[CardRank]] with
		def compare(x: List[CardRank], y: List[CardRank]): Int =
			x.zip(y)
				.map: (a, b) =>
					implicitly[Ordering[CardRank]].compare(a, b)
				.dropWhile: value =>
					value == 0
				.headOption
				.getOrElse(0)

	given Ordering[Hand] =
		Ordering.by[Hand, HandType](_.typ).orElseBy(_.cards)

object Day7Part2 extends IOApp:
	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8Lines(Path("input.txt"))
			.filter:
				_.nonEmpty
			.map:
				case s"$cards $bid" => Hand(cards.map(CardRank.fromChar).toList, bid.toInt)
			.compile.toList
			.map: hands =>
				hands
					.sorted
					.reverse
					.zipWithIndex
					.map: (hand, index) =>
						hand.bid * (1 + index)
					.sum
			.flatMap:
				IO.println
			.map:
				_ => ExitCode(0)
