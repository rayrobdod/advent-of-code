//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

import scala.annotation.tailrec

val inputFile = os.pwd / "input.txt"

enum Opcode:
	case Adv //  A / 2^V -> A
	case Bxl // B xor L -> B
	case Bst // (V % 8) -> B
	case Jnz // if A != 0 then L -> PC
	case Bxc // B xor C -> B
	case Out // write (V % 8)
	case Bdv // A / 2^V -> B
	case Cdv // A / 2^V -> C
end Opcode

object Opcode:
	// There *is* a way to use Mirror to do this...
	def fromInt(x: Int): Opcode = x match
		case 0 => Adv
		case 1 => Bxl
		case 2 => Bst
		case 3 => Jnz
		case 4 => Bxc
		case 5 => Out
		case 6 => Bdv
		case 7 => Cdv
		case _ => throw new IllegalArgumentException

	object _dv:
		def unapply(x: Opcode): Option[(Registers, Long) => Registers] =
			x match
				case Adv => Some((r, i) => r.copy(a = i))
				case Bdv => Some((r, i) => r.copy(b = i))
				case Cdv => Some((r, i) => r.copy(c = i))
				case _ => None
end Opcode

enum ComboOperand:
	case _0, _1, _2, _3, _A, _B, _C

	def value(regs: Registers): Long =
		this match
			case `_0` => 0
			case `_1` => 1
			case `_2` => 2
			case `_3` => 3
			case `_A` => regs.a
			case `_B` => regs.b
			case `_C` => regs.c
end ComboOperand

object ComboOperand:
	// There *is* a way to use Mirror to do this...
	def fromInt(x: Int): ComboOperand = x match
		case 0 => _0
		case 1 => _1
		case 2 => _2
		case 3 => _3
		case 4 => _A
		case 5 => _B
		case 6 => _C
		case _ => throw new IllegalArgumentException
end ComboOperand

final case class Registers(a: Long, b: Long, c: Long, pc: Int)

object I:
	def unapply(x:String):Option[Int] = x.toIntOption

val (initialRegs: Registers, program: Seq[Int]) = locally:
	os.read(inputFile) match
		case s"Register A: ${I(a)}\nRegister B: ${I(b)}\nRegister C: ${I(c)}\n\nProgram: $prog\n" =>
			(Registers(a, b, c, 0), prog.split(",").map(_.toInt).toSeq)

def executeProgram(initialRegs: Registers, program: Seq[Int]): Seq[Int] =
	@tailrec def impl(regs: Registers, output: List[Int]): List[Int] =
		if program.isDefinedAt(regs.pc + 1) then
			val opcode = Opcode.fromInt(program(regs.pc))
			opcode match
				case Opcode._dv(withFn) =>
					val divisorPower = ComboOperand.fromInt(program(regs.pc + 1)).value(regs)
					val divisor = 1 << divisorPower
					val newValue = regs.a / divisor
					val newRegs = withFn(regs, newValue).copy(pc = regs.pc + 2)
					impl(newRegs, output)
				case Opcode.Bxl =>
					val newValue = regs.b ^ program(regs.pc + 1)
					val newRegs = regs.copy(b = newValue, pc = regs.pc + 2)
					impl(newRegs, output)
				case Opcode.Bst =>
					val newValue = ComboOperand.fromInt(program(regs.pc + 1)).value(regs) % 8
					val newRegs = regs.copy(b = newValue, pc = regs.pc + 2)
					impl(newRegs, output)
				case Opcode.Bxc =>
					val newValue = regs.b ^ regs.c
					val newRegs = regs.copy(b = newValue, pc = regs.pc + 2)
					impl(newRegs, output)
				case Opcode.Jnz =>
					val newPc =
						if 0 == regs.a then
							regs.pc + 2
						else
							program(regs.pc + 1)
					val newRegs = regs.copy(pc = newPc)
					impl(newRegs, output)
				case Opcode.Out =>
					val newOutput = (ComboOperand.fromInt(program(regs.pc + 1)).value(regs) % 8).toInt
					val newRegs = regs.copy(pc = regs.pc + 2)
					impl(newRegs, newOutput :: output)
		else
			output
	end impl

	impl(initialRegs, List.empty).reverse
end executeProgram

val part1 = executeProgram(initialRegs, program).mkString(",")

println(s"part 1: $part1")

// I ended up transpiling my input program to figure out the following properties:

// The input program contains only one jump, a `Jnz 0` at the end of the program.
// Each loop of the input program only modifies A with a single `Adv 8`.
// Each loop prints exactly one time
// B and C's state at the end of one loop does not affect the next loop.

// It might be possible to write findQuine in terms of the interpreter if those properties can be assumed,
// but I had already transpiled to figure out these properties,
// and I only have to figure out the answer for my input
// so I was lazy and plugged the transpiled version into findQuine

def executeProgram2(initialA: Long): Seq[Int] =
	@tailrec def impl(a:Long, out: List[Int]): List[Int] =
		if a == 0 then
			out
		else
			var b: Int = (a % 8).toInt
			b ^= 3
			val c: Int = ((a / (1 << b)) % 8).toInt
			b ^= 5
			b ^= c
			impl(a / 8, b.toInt :: out)

	impl(initialA, Nil).reverse
end executeProgram2

def findQuine: Seq[Long] =
	program
		.foldRight(Seq(0L)): (output, as) =>
			as.flatMap: a =>
				val bs = (0 until 8).filter: b2 =>
					val preA = (a * 8) + b2

					var b: Int = b2
					b ^= 3
					val c: Int = ((preA / (1 << b)) % 8).toInt
					b ^= 5
					b ^= c
					b == output
				bs.map(b => a * 8 + b)
end findQuine

println(s"part 2: ${findQuine.min}")
