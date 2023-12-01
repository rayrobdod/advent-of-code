object Day1Part1 {
	
	def firstAndLastDigitsInString(line:String):Int = {
		val firstLast = line.chars()
			.filter(Character.isDigit)
			.collect(
				() => Array[Int](-1, -1),
				(folding, value) => {
					if (folding(0) == -1) {folding(0) = value}
					folding(1) = value
				},
				(left, right) => throw new Exception()
			)
		
		Integer.parseInt(new String(firstLast, 0, 2))
	}
	
	def main(args:Array[String]):Unit = {
		System.out.println(
			java.nio.file.Files.lines(java.nio.file.Path.of("input.txt"))
				.mapToInt(firstAndLastDigitsInString)
				.sum
		)
	}
}
