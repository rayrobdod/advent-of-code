//> using scala 3.3.4
//> using dep com.lihaoyi::os-lib:0.11.3
val inputFile = os.pwd / "input.txt"
val input = os.read(inputFile)


def parse(input: String): (Seq[String], Seq[String]) =
  val lines = input.linesIterator.toSeq
  val towels = lines(0).split(", ").toSeq
  val patterns = lines.drop(2)
  (towels, patterns)
end parse

def canCreatePattern(towels: Seq[String])(pattern: String): Boolean =
  if pattern.isEmpty then
    true
  else
    towels.exists: towel =>
      pattern.startsWith(towel)
        && canCreatePattern(towels)(pattern.stripPrefix(towel))
end canCreatePattern

def part1(input: String) =
  val (towels, patterns) = parse(input)

  patterns.count:
    canCreatePattern(towels)
end part1



def waysToCreatePattern(towels: Seq[String])(pattern: String): Long =
  def impl(restOfPattern: String, restOfTowelsFrequency: Map[String, Long]): Long =
    if restOfPattern.isEmpty then
      restOfTowelsFrequency.getOrElse("", 0L)
    else
      val newTowelsFrequency = restOfTowelsFrequency.getOrElse("", 0L)

      val restOfOrNewTowelsFrequency =
        towels.foldLeft(restOfTowelsFrequency): (folding, newTowel) =>
          folding.updatedWith(newTowel): restOfTowelFrequency =>
            restOfTowelFrequency
              .orElse(Option(0L))
              .map(_ + newTowelsFrequency)

      val tailTowelsFrequency = restOfOrNewTowelsFrequency.collect:
        case (towelPart, frequency)
          if towelPart.nonEmpty && towelPart.charAt(0) == restOfPattern.charAt(0) =>
            (towelPart.substring(1), frequency)

      impl(restOfPattern.substring(1), tailTowelsFrequency)
  end impl

  impl(pattern, Map("" -> 1L))
end waysToCreatePattern

def part2(input: String) =
  val (towels, patterns) = parse(input)
  patterns
    .map:
      waysToCreatePattern(towels)
    .sum
end part2

println(s"part 1: ${part1(input)}")
println(s"part 2: ${part2(input)}")
