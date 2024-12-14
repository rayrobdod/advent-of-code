//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

val areaWidth = 101 // 11
val areaHeight = 103 // 7

enum Quadrant:
	case NE, NW, SE, SW, None

case class WrappingVector(x: Int, y: Int):
	def +(other: WrappingVector): WrappingVector =
		WrappingVector(
			(this.x + other.x + areaWidth) % areaWidth,
			(this.y + other.y + areaHeight) % areaHeight,
		)
	end +
	def *(n: Int): WrappingVector =
		val retval = WrappingVector(
			(this.x * n) % areaWidth,
			(this.y * n) % areaHeight
		)
		retval
	end *

	def quadrant: Quadrant =
		import Quadrant.*
		if y == areaHeight / 2 || x == areaWidth / 2 then
			None
		else if x < areaWidth / 2 then
			if y < areaHeight / 2 then
				NW
			else
				NE
		else
			if y < areaHeight / 2 then
				SW
			else
				SE
	end quadrant

case class Robot(p: WrappingVector, v: WrappingVector):
	def pAt(n: Int): WrappingVector = p + v * n

object L:
	def unapply(x:String):Option[Int] = x.toIntOption

val robots: Seq[Robot] =
	os.read.lines(os.pwd / "input.txt")
		.map:
			_.strip
		.map:
			case s"p=${L(px)},${L(py)} v=${L(vx)},${L(vy)}" =>
				Robot(WrappingVector(px, py), WrappingVector(vx, vy))

val part1 = robots
	.map:
		_.pAt(100)
	.groupBy:
		_.quadrant
	.-(Quadrant.None)
	.map: (_, robotsInQuadrant) =>
		robotsInQuadrant.size
	.product

println(s"part 1: ${part1}")

// x repeats on an areaWidth cycle, y repeats on an areaHeight cycle
// (x,y) repeats on a lcm(x, y) cycle, which since 101 and 103 are coprime, is x * y
val part2 = (0 until areaWidth * areaHeight).filter: n =>
	val ps = robots.map(_.pAt(n)).toSet
	ps.exists: p =>
		(1 to 4).forall: dx =>
			Seq(-dx, dx).forall: dy =>
				ps.contains(p + WrappingVector(dx, dy))
		/*
		(-2 to 2).forall: dx =>
			(-2 to 2).forall: dy =>
				ps.contains(p + WrappingVector(dx, dy))
		*/

part2.foreach: n =>
	val rs = robots.map(_.pAt(n))
	(0 until areaHeight).foreach: y =>
		(0 until areaWidth).foreach: x =>
			val p = WrappingVector(x, y)
			val cnt = rs.count(_ == p)
			print(if (0 == cnt) '.' else cnt)
		println
	println

println(s"part 2: ${part2}")
