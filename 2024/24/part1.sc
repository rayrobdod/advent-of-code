//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

val inputFile = os.pwd / "input.txt"

sealed trait WireValueOpt
object WireValueOpt:
	case object None extends WireValueOpt

sealed trait WireValue extends WireValueOpt
object WireValue:
	case object _0 extends WireValue
	case object _1 extends WireValue

enum Op:
	case And, Or, Xor

	def apply(left: WireValue, right: WireValue): WireValue =
		import WireValue.*
		this match
			case And => if left == _1 && right == _1 then _1 else _0
			case Or => if left == _0 && right == _0 then _0 else _1
			case Xor => if left == right then _0 else _1

final case class Gate(op: Op, from1: String, from2: String, to: String)


val (inputWireValues: Map[String, WireValue], gates: Seq[Gate]) =
	val lines = os.read.lines(inputFile)
	val splitAtIndex = lines.indexOf("")
	val (initialWireValuesLines, gatesLines) = lines.splitAt(splitAtIndex)

	val initialWireValues = initialWireValuesLines
		.map:
			case s"$name: 0" => (name, WireValue._0)
			case s"$name: 1" => (name, WireValue._1)
		.toMap

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

	(initialWireValues, gates)

val gateOutputWires = gates.map:
	case Gate(_, _, _, out) => out

val initialWireValues: Map[String, WireValueOpt] =
	inputWireValues ++ gateOutputWires.map(_ -> WireValueOpt.None)

def zValue(wireValues: Map[String, WireValueOpt]): Option[Long] =
	wireValues
		.filter(_._1.charAt(0) == 'z')
		.toList
		.sortBy(_._1)
		.reverse
		.map(_._2)
		.foldLeft(Option(0L)): (folding, bit) =>
			(folding, bit) match
				case (None, _) => None
				case (_, WireValueOpt.None) => None
				case (Some(foldingValue), WireValue._0) => Some(foldingValue * 2 + 0)
				case (Some(foldingValue), WireValue._1) => Some(foldingValue * 2 + 1)

var wireValues: Map[String, WireValueOpt] = initialWireValues
while ! zValue(wireValues).isDefined do
	wireValues = gates.foldLeft(wireValues): (wireValues, gate) =>
		val op = gate.op
		wireValues(gate.from1) match
			case in1: WireValue =>
				wireValues(gate.from2) match
					case in2: WireValue =>
						val out = op(in1, in2)
						wireValues.updated(gate.to, out)
					case WireValueOpt.None => wireValues
			case WireValueOpt.None => wireValues

println(s"part 1: ${zValue(wireValues).get}")
