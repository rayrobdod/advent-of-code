//> using scala 3.3.1
//> using dep org.typelevel::toolkit:0.1.19

import cats.syntax.all._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.parse.Parser
import fs2.io.file.Files
import fs2.io.file.Path

trait LongPartialFunction:
	def apply(a:Long):Long
	def isDefinedAt(a:Long):Boolean

	/*
	 * The inability to say "a partial function orElse a total function is a total function"
	 * is the primary reason I'm defining a new PartialFunction instead of using the standard one
	 */
	def orElse(that: LongFunction):LongFunction =
		{(b:Long) => if (this.isDefinedAt(b)) {this.apply(b)} else {that(b)}}

	def orElse(that: LongPartialFunction):LongPartialFunction =
		val self = this
		new LongPartialFunction:
			override def apply(b:Long):Long =
				if (self.isDefinedAt(b)) {self(b)} else {that(b)}
			override def isDefinedAt(b:Long):Boolean =
				self.isDefinedAt(b) || that.isDefinedAt(b)
end LongPartialFunction

object LongPartialFunction:
	private final class OrElseAppender(first: LongPartialFunction) extends cats.parse.Appender[LongPartialFunction, LongPartialFunction]:
		private var current: LongPartialFunction = first

		override def append(item: LongPartialFunction): this.type =
			this.current = this.current orElse item
			this

		override def finish(): LongPartialFunction =
			this.current

	object orElseAccumulator extends cats.parse.Accumulator[LongPartialFunction, LongPartialFunction]:
		override def newAppender(first: LongPartialFunction): cats.parse.Appender[LongPartialFunction, LongPartialFunction] =
			new OrElseAppender(first)
end LongPartialFunction

trait LongFunction:
	def apply(a:Long):Long

	def andThen(that: LongFunction): LongFunction =
		{b => that(this(b))}

object LongFunction:
	def identity:LongFunction = {b => b}

final case class Almanac(
	seeds: List[Long],
	seedToSoilMap: LongFunction,
	soilToFertilizerMap: LongFunction,
	fertilizerToWaterMap: LongFunction,
	waterToLightMap: LongFunction,
	lightToTemperatureMap: LongFunction,
	temperatureToHumidityMap: LongFunction,
	humidityToLocationMap: LongFunction,
):
	def seedToLocationMap: LongFunction =
		seedToSoilMap andThen
			soilToFertilizerMap andThen
			fertilizerToWaterMap andThen
			waterToLightMap andThen
			lightToTemperatureMap andThen
			temperatureToHumidityMap andThen
			humidityToLocationMap
end Almanac


object Almanac:
	def parser: Parser[Almanac] =
		val long = cats.parse.Numbers.nonNegativeIntString.map(_.toLong)
		val sp = Parser.string(" ")
		val lf = Parser.string("\n")
		val lflf = Parser.string("\n\n")
		val anyLine = Parser.charsWhile(_ != '\n') *> Parser.string("\n")

		val seedsParser = Parser.string("seeds: ") *> long.repSep(sp).map(_.toList) <* lflf

		val mappingParser =
			anyLine *>
			((long <* sp) ~ (long <* sp) ~ (long <* lf))
				.map: line =>
					val ((destinationRangeStart, sourceRangeStart), length) = line
					new LongPartialFunction:
						override def apply(source:Long):Long =
							source - sourceRangeStart + destinationRangeStart
						override def isDefinedAt(source:Long):Boolean =
							sourceRangeStart <= source && source <= (sourceRangeStart + length)
				.repAs(using LongPartialFunction.orElseAccumulator)
				.map(_ orElse LongFunction.identity) <*
			lf

		(seedsParser ~ mappingParser ~ mappingParser ~ mappingParser ~ mappingParser ~ mappingParser ~ mappingParser ~ mappingParser).map: data =>
			val (((((((seeds, map1), map2), map3), map4), map5), map6), map7) = data
			Almanac(
				seeds,
				map1,
				map2,
				map3,
				map4,
				map5,
				map6,
				map7,
			)
end Almanac

object Day5Part1 extends IOApp:
	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8(Path("input.txt"))
			.compile.foldMonoid
			.map(Almanac.parser.parseAll)
			.map(_.toOption.get)
			.map: almanac =>
				almanac.seeds.map(almanac.seedToLocationMap.apply).min
			.flatMap(IO.println)
			.map(_ => ExitCode(0))
