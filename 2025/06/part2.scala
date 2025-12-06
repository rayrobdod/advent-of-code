//> using scala 3.7.2
//> using dep com.lihaoyi::os-lib:0.11.6

enum Op:
	case None, Sum, Product

object Op:
	def fromChar(c: Char): Op =
		c match
			case ' ' => Op.None
			case '+' => Op.Sum
			case '*' => Op.Product

case class Result(combinedSum: Long, problemNums: List[Long]):
	def push(x: (Long, Op)): Result =
		val (n, op) = x
		val problemNums2 = n :: problemNums
		op match
			case Op.None => Result(combinedSum, problemNums2)
			case Op.Sum => Result(combinedSum + problemNums2.sum, Nil)
			case Op.Product => Result(combinedSum + problemNums2.product, Nil)

object Day6Part2:
	def main(args: Array[String]): Unit =
		val lines: Seq[String] = os.read.lines(os.pwd / "input.txt")

		val result = lines
			.transpose
			.filter: row =>
				row.exists: c =>
					c != ' '
			.map: row =>
				((row.init.mkString.strip.toLong, Op.fromChar(row.last)))
			.reverse
			.foldLeft(Result(0, Nil)): (folding, row) =>
				folding.push(row)
			.combinedSum

		System.out.println(s"part 2: ${result}")
