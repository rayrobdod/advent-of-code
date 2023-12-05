//> using scala 3.3.1
//> using dep org.typelevel::toolkit:0.1.19

import cats.syntax.all._
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.ExitCode
import cats.parse.Parser
import fs2.io.file.Files
import fs2.io.file.Path

sealed trait Range:
	def mapStart(fn: Long => Long): Range

object EmptyRange extends Range:
	override def mapStart(fn: Long => Long): EmptyRange.type =
		EmptyRange

final case class NonEmptyRange(start: Long, length: Long) extends Range:
	if (length < 1) throw new IllegalArgumentException("negative length")

	def end:Long = start + length - 1

	def &(that: NonEmptyRange): Range =
		val newStart = math.max(this.start, that.start)
		val newEnd = math.min(this.end, that.end)
		if newEnd <= newStart then
			EmptyRange
		else
			NonEmptyRange(newStart, newEnd - newStart + 1)
		end if

	def ~&(that: NonEmptyRange): Seq[NonEmptyRange] =
		this & that match
			case EmptyRange => Seq(this)
			case intersect:NonEmptyRange =>
				if intersect.start == this.start then
					if intersect.length == this.length then
						Seq.empty
					else
						Seq(NonEmptyRange(intersect.end, this.end - intersect.end))
				else
					if intersect.end == this.end then
						Seq(NonEmptyRange(this.start, intersect.start - this.start))
					else
						Seq(
							NonEmptyRange(this.start, intersect.start - this.start),
							NonEmptyRange(intersect.end, this.end - intersect.end),
						)


	override def mapStart(fn: Long => Long): NonEmptyRange =
		NonEmptyRange(fn(this.start), this.length)

end NonEmptyRange

final case class AlmanacMapping1(
	destinationRangeStart: Long,
	sourceRangeStart: Long,
	length: Long,
):
	def sourceRangeEnd: Long = sourceRangeStart + length
	def destinationRangeEnd: Long = destinationRangeStart + length
	def sourceRange: NonEmptyRange = NonEmptyRange(sourceRangeStart, length)
	def destinationRange: NonEmptyRange = NonEmptyRange(destinationRangeStart, length)

	def apply(in: NonEmptyRange): Range =
		(this.sourceRange & in).mapStart(x => x - sourceRangeStart + destinationRangeStart)

final class AlmanacMapping(from:String, to:String, nonidentityParts: Iterable[AlmanacMapping1]):
	private def identityParts:Iterable[AlmanacMapping1] =
		val fullRange = NonEmptyRange(0, 0xFFFF_FFFFL)
		nonidentityParts
			.map(_.sourceRange)
			.foldLeft(Seq(fullRange)): (identityRanges, sourceRange) =>
				identityRanges.flatMap(_ ~& sourceRange)
			.map: identityRange =>
				val NonEmptyRange(start, length) = identityRange
				AlmanacMapping1(start, start, length)

	private val parts = nonidentityParts ++ identityParts

	def apply(sourceRanges: Iterable[NonEmptyRange]): Iterable[NonEmptyRange] =
		//System.out.println(s"$from to $to")
		//System.out.println(s"<- $sourceRanges")
		val retval = parts.flatMap: part =>
			sourceRanges
				.map: sourceRange =>
					part(sourceRange)
				.collect:
					{case destRange:NonEmptyRange => destRange}
		//System.out.println(s"-> $retval")
		retval

object AlmanacMapping:
	def apply(from:String, to:String, nonidentityParts: Iterable[AlmanacMapping1]): AlmanacMapping =
		new AlmanacMapping(from, to, nonidentityParts)

final case class Almanac(
	seeds: Iterable[NonEmptyRange],
	seedToSoilMap: AlmanacMapping,
	soilToFertilizerMap: AlmanacMapping,
	fertilizerToWaterMap: AlmanacMapping,
	waterToLightMap: AlmanacMapping,
	lightToTemperatureMap: AlmanacMapping,
	temperatureToHumidityMap: AlmanacMapping,
	humidityToLocationMap: AlmanacMapping,
):
	def locations: Iterable[NonEmptyRange] =
		humidityToLocationMap(
			temperatureToHumidityMap(
				lightToTemperatureMap(
					waterToLightMap(
						fertilizerToWaterMap(
							soilToFertilizerMap(
								seedToSoilMap(
									seeds
								)
							)
						)
					)
				)
			)
		)
end Almanac


object Almanac:
	def parser: Parser[Almanac] =
		val long = cats.parse.Numbers.nonNegativeIntString.map(_.toLong)
		val sp = Parser.string(" ")
		val lf = Parser.string("\n")
		val lflf = Parser.string("\n\n")
		val word = Parser.charsWhile(c => 'a' <= c && c <= 'z')

		val seedsParser =
			Parser.string("seeds: ") *>
			((long <* sp) ~ long)
				.map(NonEmptyRange.apply)
				.repSep(sp)
				.map(_.toList) <*
			lflf

		val mappingParser =
			((((word <* Parser.string("-to-")) ~ (word <* Parser.string(" map:\n"))) ~
			(
				((long <* sp) ~ (long <* sp) ~ (long <* lf))
					.map: line =>
						val ((destinationRangeStart, sourceRangeStart), length) = line
						AlmanacMapping1(destinationRangeStart, sourceRangeStart, length)
					.rep
					.map(_.toList)
			))
				.map: data =>
					val ((from, to), parts) = data
					AlmanacMapping(from, to, parts)
			) <*
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

object Day5Part2 extends IOApp:
	def run(args:List[String]): IO[ExitCode] =
		Files[IO].readUtf8(Path("input.txt"))
			.compile.foldMonoid
			.map(Almanac.parser.parseAll)
			.map(_.toOption.get)
			.map(_.locations)
			.map(_.map(_.start).min)
			.flatMap(IO.println)
			.map(_ => ExitCode(0))
