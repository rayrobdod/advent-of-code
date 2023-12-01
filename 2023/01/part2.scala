
object Day1Part2 {
	
	val digits:Map[String, Int] = Map(
		"1" -> 1,
		"2" -> 2,
		"3" -> 3,
		"4" -> 4,
		"5" -> 5,
		"6" -> 6,
		"7" -> 7,
		"8" -> 8,
		"9" -> 9,
		"0" -> 0,
		"one" -> 1,
		"two" -> 2,
		"three" -> 3,
		"four" -> 4,
		"five" -> 5,
		"six" -> 6,
		"seven" -> 7, 
		"eight" -> 8,
		"nine" -> 9,
		"zero" -> 0,
	)
	
	
	def firstAndLastDigitsInString(line:String):Int = {
		var first:Int = -1
		var last:Int = -1
		
		(0 until line.length).foreach({i =>
			digits.foreach({(k, v) =>
				if (line.drop(i).startsWith(k)) {
					last = v
					if (-1 == first) {
						first = v
					}
				}
			})
		})
		
		if (first == -1 || last == -1) {
			throw new Exception(s"$line has no digits")
		}
		first * 10 + last
	}
	
	def main(args:Array[String]):Unit = {
		System.out.println(
			java.nio.file.Files.lines(java.nio.file.Path.of("input.txt"))
				.mapToInt(firstAndLastDigitsInString)
				.sum
		)
	}
}
