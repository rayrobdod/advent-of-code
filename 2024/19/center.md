## Solution Summary

1. Parse the input into a list of towels and a list of desired patterns
2. For each pattern,
3. Optimize the data representation

### Parsing

The input consists of one line of comma-separated towels, one blank line, then a number of lines that each represent a desired pattern.
So, convert the input string into a sequence of lines, take the first line and `split` the line to get the list of towels,
and take all lines except for the first two lines, the towels line and the blank separator line, as the list of desired patterns.

```scala
def parse(input: String): (Seq[String], Seq[String]) =
  val lines = input.linesIterator.toSeq
  val towels = lines(0).split(", ").toSeq
  val patterns = lines.drop(2)
  (towels, patterns)
end parse
```

### Part 1

Patterns are created by lining up towels sequentially - that is, a valid pattern can be created by taking a valid pattern, where a pattern consisting of no towels is a valid pattern, and adding another towel to the pattern.
Reversing this, a pattern can be created from the avaliable towels if the pattern is the empty pattern or if the start of the pattern matches an avaliable towel and the rest of the pattern is a valid pattern.
This can be translated into the following recursive function:

```scala
def canCreatePattern(towels: Seq[String])(pattern: String): Boolean =
  if pattern.isEmpty then
    true
  else
    towels.exists: towel =>
      pattern.startsWith(towel)
        && canCreatePattern(towels)(pattern.stripPrefix(towel))
end canCreatePattern
```

We can then call this function for each candidate pattern,
and `count` the number of candidate patterns for which the `canCreatePattern` method returns true

```scala
def part1(input: String) =
  val (towels, patterns) = parse(input)

  patterns.count:
    canCreatePattern(towels)
end part1
```

### Part 2

Part 2 asks not just whether a pattern is able to be created, but how many different ways a pattern can be created.
It quickly becomes apparent that there is a combinatorial explosion and
that trying to use the same depth-first search strategy as part 1 will take too long.


TODO: Verbalize the logical leap that results in a `Map[String, Long]`


The towels are different lengths, and we'd like to iterate over every towel simultaneously.
So, instead of iterating over towels, the representation will iterate over individual stripes instead of over whole towels.



As before, create a recursive function. Due to the additional parameter that is passed through the recursion but which callers shouldn't be concerned with, the recursive function will be an inner function with the `waysToCreatePattern` itself being a wrapper that calls the actual recursive method.

The base case, as before, is when the pattern is zero, in which case the result is the number of ways to reach this end-of-pattern in a way that is also at the end of a towel - that is when the rest of the towel is empty.

```scala
def waysToCreatePattern(towels: Seq[String])(pattern: String): Long =
  def impl(restOfPattern: String, restOfTowelFrequency: Map[String, Long]): Long =
    if restOfPattern.isEmpty then
      restOfTowelFrequency.getOrElse("", 0L)
```

In the recursive case, first convert the frequency of having reached the end of a towel to the frequency of starting a new towel, by adding the towel-ending frequency to each whole-towel frequency.

```scala
    else
      val newTowelsFrequency = restOfTowelsFrequency.getOrElse("", 0L)

      val restOfOrNewTowelsFrequency =
        towels.foldLeft(restOfTowelsFrequency): (folding, newTowel) =>
          folding.updatedWith(newTowel): restOfTowelFrequency =>
            restOfTowelFrequency
              .orElse(Option(0L))
              .map(_ + newTowelsFrequency)
```

Then go through the towel frequencies.
Discard any towels whose next stripe does not match the next stripe of the pattern.
Then remove the next stripe from the pattern and all partial towels, and recurse.

```scala
      val tailTowelsFrequency = restOfOrNewTowelsFrequency.collect:
        case (towelPart, frequency)
          if towelPart.nonEmpty && towelPart.charAt(0) == restOfPattern.charAt(0) =>
            (towelPart.substring(1), frequency)

      impl(restOfPattern.substring(1), tailTowelsFrequency)
  end impl
```

The wrapper method calls the recursive method using the full pattern,
and a restOfTowelFrequency representing reaching the end of a towel in one way.

```scala
  impl(pattern, Map("" -> 1L))
end waysToCreatePattern
```

To calculate the result, we call this method on each of the input patterns, and sum the results.

```scala
def part2(input: String) =
  val (towels, patterns) = parse(input)
  patterns
    .map:
      waysToCreatePattern(towels)
    .sum
end part2
```

### Final Code

```scala
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
```
