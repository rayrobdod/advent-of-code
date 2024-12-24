//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

import scala.collection.mutable

val inputFile = os.pwd / "input.txt"

enum Op:
	case And, Or, Xor

final case class Gate(op: Op, from1: String, from2: String, to: String)

val gates: Seq[Gate] =
	val lines = os.read.lines(inputFile)
	val splitAtIndex = lines.indexOf("")
	val (initialWireValuesLines, gatesLines) = lines.splitAt(splitAtIndex)

	val gates = gatesLines.tail
		.map:
			case s"$left $op $right -> $to" =>
				Gate(
					op match {
						case "AND" => Op.And
						case "OR" => Op.Or
						case "XOR" => Op.Xor
					},
					left,
					right,
					to,
				)

	// look at the printed output;
	// figure out what is wrong
	//	(A correct output is repetitive, the error should be near where the repetition breaks down)
	//	(A `z` wire should not have a nickname.)
	// swap the two wires here
	// repeat three more times

	// It's a relatively manual process, but still got me my best rank this year
	val gates2 = gates.map:
		/*
		e.g.
		case Gate(a,b,c,"aaa") => Gate(a,b,c,"aoc")
		case Gate(a,b,c,"aoc") => Gate(a,b,c,"aaa")

		case Gate(a,b,c,"bbb") => Gate(a,b,c,"ccc")
		case Gate(a,b,c,"ccc") => Gate(a,b,c,"bbb")
		*/

		case g => g

	gates2


val nicknames = mutable.Map.empty[String, String]

object Nicknamed:
	def unapply(wire: String): Option[String] = nicknames.get(wire)

gates.foreach:
	case Gate(Op.Xor, s"x$_1", s"y$_2", to) if _1 == _2 =>
		nicknames(to) = s"x⊻y$_1"
	case Gate(Op.Xor, s"y$_1", s"x$_2", to) if _1 == _2 =>
		nicknames(to) = s"x⊻y$_1"

	case Gate(Op.And, s"x00", s"y00", to) =>
		nicknames(to) = s"c01"

	case Gate(Op.And, s"x$_1", s"y$_2", to) if _1 == _2 =>
		nicknames(to) = s"x∧y$_1"
	case Gate(Op.And, s"y$_1", s"x$_2", to) if _1 == _2 =>
		nicknames(to) = s"x∧y$_1"

	case _ =>
		//

(0 until 1000).foreach: _ =>
	gates.foreach:
		case Gate(Op.And, Nicknamed(s"x⊻y$_1"), Nicknamed(s"c$_2"), to) if _1 == _2 =>
			nicknames(to) = s"x⊻y∧c$_1"
		case Gate(Op.And, Nicknamed(s"c$_2"), Nicknamed(s"x⊻y$_1"), to) if _1 == _2 =>
			nicknames(to) = s"x⊻y∧c$_1"

		case Gate(Op.Or, Nicknamed(s"x⊻y∧c$_1"), Nicknamed(s"x∧y$_2"), to) if _1 == _2 =>
			nicknames(to) = s"c${s"0${_1.toInt + 1}".takeRight(2)}"
		case Gate(Op.Or, Nicknamed(s"x∧y$_2"), Nicknamed(s"x⊻y∧c$_1"), to) if _1 == _2 =>
			nicknames(to) = s"c${s"0${_1.toInt + 1}".takeRight(2)}"

		case _ =>
			//


gates
	.sortBy: gate =>
		val to = nicknames.getOrElse(gate.to, gate.to)
		to.takeRight(2) + to.dropRight(2)

	.map: gate =>
		val sb = StringBuilder()
		sb.append(gate.from1)
		nicknames.get(gate.from1) match
			case Some(nickname) => sb.append(s"($nickname)")
			case None => //
		sb.append:
			gate.op match
				case Op.And => " AND "
				case Op.Or => " OR "
				case Op.Xor => " XOR "
		sb.append(gate.from2)
		nicknames.get(gate.from2) match
			case Some(nickname) => sb.append(s"($nickname)")
			case None => //
		sb.append(" -> ")
		sb.append(gate.to)
		nicknames.get(gate.to) match
			case Some(nickname) => sb.append(s"($nickname)")
			case None => //
		sb.toString
	.foreach:
		println
