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

def part1(input: String) =
  val (towels, patterns) = parse(input)

  def canCreatePattern(restOfPattern: String): Boolean =
    if restOfPattern.isEmpty then
      true
    else
      towels.exists: towel =>
        restOfPattern.startsWith(towel)
          && canCreatePattern(restOfPattern.stripPrefix(towel))

  patterns.count: pattern =>
    canCreatePattern(pattern)
end part1

def part2(input: String) =
  val (towels, patterns) = parse(input)
  patterns
    .map: pattern =>
      def impl(pattern: String, towelParts: Map[String, Long]): Long =
        if pattern.isEmpty then
          towelParts.getOrElse("", 0L)
        else
          val nextTowelParts =
            val advancePatternTowelParts = towelParts.collect:
              case (towelPart, frequency)
                  if towelPart.nonEmpty && towelPart.charAt(0) == pattern.charAt(0) =>
                (towelPart.substring(1), frequency)

            val newTowelFrequency = advancePatternTowelParts.getOrElse("", 0L)

            if 0 == newTowelFrequency then
              advancePatternTowelParts
            else
              towels.foldLeft(advancePatternTowelParts): (folding, newTowel) =>
                folding.updated(
                  newTowel,
                  folding.getOrElse(newTowel, 0L) + newTowelFrequency
                )

          impl(pattern.substring(1), nextTowelParts)
      impl(pattern, towels.map(_ -> 1L).toMap)
    .sum
end part2

println(s"part 1: ${part1(input)}")
println(s"part 2: ${part2(input)}")
