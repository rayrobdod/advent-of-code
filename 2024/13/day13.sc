//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

final case class Vector(x: Long, y: Long):
	def unary_- : Vector = Vector(-x, -y)
	def +(v: Vector): Vector = Vector(this.x + v.x, this.y + v.y)
	def -(v: Vector): Vector = Vector(this.x - v.x, this.y - v.y)
	def *(i: Long): Vector = Vector(this.x * i, this.y * i)
end Vector

final case class Rational(num: Long, denom: Long):
	def unary_- : Rational = Rational(-num, denom)
	def +(f: Rational): Rational = Rational(this.num * f.denom + f.num * this.denom, this.denom * f.denom)
	def -(f: Rational): Rational = this + (-f)
	def /(f: Rational): Rational = Rational(this.num * f.denom, f.num * this.denom)
	def toLongOption: Option[Long] = Option.when(0 != denom && 0 == num % denom)(num / denom)
end Rational

val aPrice = 3
val bPrice = 1

final case class ButtonPresses(a: Long, b: Long):
	def cost: Long = a * aPrice + b * bPrice

final case class ClawMachine(
	buttonA: Vector,
	buttonB: Vector,
	prize: Vector,
):
	def pressWinPossibilities1: Seq[ButtonPresses] =
		for
			aPresses <- 0 until 100;
			bPresses <- 0 until 100
			if prize == buttonA * aPresses + buttonB * bPresses
		yield
			ButtonPresses(aPresses, bPresses)
	end pressWinPossibilities1

	def pressWinPossibilities2: Option[ButtonPresses] =
		// experimentation shows that part 1 has only one possible button press solution per machine
		// lets assume the same holds for part 2

		val bPresses = ((Rational(prize.y, buttonA.y) - Rational(prize.x, buttonA.x))
				/ (Rational(buttonB.y, buttonA.y) - Rational(buttonB.x, buttonA.x)))

		val aPresses = bPresses.toLongOption
			.flatMap: bPresses2 =>
				Rational(prize.x - buttonB.x * bPresses2, buttonA.x).toLongOption

		for
			a <- aPresses;
			b <- bPresses.toLongOption
		yield
			ButtonPresses(a, b)
	end pressWinPossibilities2
end ClawMachine

object IsClawMachine:
	def unapply(s: String): Option[ClawMachine] =
		s match
			case s"Button A: X+${ax}, Y+${ay}\nButton B: X+${bx}, Y+${by}\nPrize: X=${px}, Y=${py}" =>
				Some(
					ClawMachine(
						buttonA = Vector(ax.toInt, ay.toInt),
						buttonB = Vector(bx.toInt, by.toInt),
						prize = Vector(px.toInt, py.strip.toInt),
					)
				)
			case _ => None

val clawMachines: Seq[ClawMachine] =
	os.read(os.pwd / "input.txt")
		.split("\n\n")
		.view
		.map:
			case IsClawMachine(cm) => cm
		.toSeq

val part1 = clawMachines
	.map: cm =>
		cm.pressWinPossibilities1
			.map:
				_.cost
			.minOption
			.getOrElse(0L)
	.sum

println(s"part 1: ${part1}")

val part2Offset = Vector(10000000000000, 10000000000000)

val clawMachines2: Seq[ClawMachine] =
	clawMachines.map(cm => cm.copy(prize = cm.prize + part2Offset))

val part1v2 = clawMachines
	.map: cm =>
		cm.pressWinPossibilities2
			.map:
				_.cost
			.minOption
			.getOrElse(0L)
	.sum

println(s"part 1: ${part1v2}")

val part2 = clawMachines2
	.map: cm =>
		cm.pressWinPossibilities2
			.map:
				_.cost
			.minOption
			.getOrElse(0L)
	.sum

println(s"part 2: ${part2}")
