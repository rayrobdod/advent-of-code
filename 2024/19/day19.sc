//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

val inputFile = os.pwd / "input.txt"

val (avaliables: List[String], desireds: List[String]) =
	val split = os.read(inputFile).split("\n\n")
	(split(0).split(", ").toList, split(1).linesIterator.toList)

val part1 = desireds
	.filter: desired =>
		def impl(remainingDesired: String): Boolean =
			if "" == remainingDesired then
				true
			else
				avaliables
					.filter: avaliable =>
						remainingDesired.startsWith(avaliable)
					.find: avaliable =>
						impl(remainingDesired.drop(avaliable.size))
					.isDefined
		impl(desired)
	.size

println(s"part 1 v1: $part1")

sealed trait Trie:
	def value: Long
	def w: Trie
	def u: Trie
	def b: Trie
	def r: Trie
	def g: Trie

	def getTrie(c: Char): Trie =
		c match
			case 'w' => this.w
			case 'u' => this.u
			case 'b' => this.b
			case 'r' => this.r
			case 'g' => this.g
	end getTrie

	private def copy(
		value: Long = this.value,
		w: Trie = this.w,
		u: Trie = this.u,
		b: Trie = this.b,
		r: Trie = this.r,
		g: Trie = this.g,
	): Trie =
		Trie.Node(value, w, u, b, r, g)


	def +(elem: String): Trie =
		if "" == elem then
			this.copy(value = this.value + 1)
		else
			elem.charAt(0) match
				case 'w' => this.copy(w = this.w + elem.substring(1))
				case 'u' => this.copy(u = this.u + elem.substring(1))
				case 'b' => this.copy(b = this.b + elem.substring(1))
				case 'r' => this.copy(r = this.r + elem.substring(1))
				case 'g' => this.copy(g = this.g + elem.substring(1))
	end +

	def apply(elem: String): Long =
		if "" == elem then
			value
		else
			getTrie(elem.charAt(0)).apply(elem.substring(1))
	end apply

	def isEmpty: Boolean

	def nonEmpty: Boolean = ! this.isEmpty

	def map(fn: Long => Long): Trie

	def |+|(other: Trie): Trie =
		if this.isEmpty then
			other
		else if other.isEmpty then
			this
		else
			Trie.Node(
				this.value + other.value,
				this.w |+| other.w,
				this.u |+| other.u,
				this.b |+| other.b,
				this.r |+| other.r,
				this.g |+| other.g,
			)
	end |+|

	protected def addString1(current: String, b: StringBuilder, mid: String): b.type =
		if 0 != this.value then
			b.append(current)
			b.append(" -> ")
			b.append(this.value)
			b.append(mid)
		this.w.addString1(current + 'w', b, mid)
		this.u.addString1(current + 'u', b, mid)
		this.b.addString1(current + 'b', b, mid)
		this.r.addString1(current + 'r', b, mid)
		this.g.addString1(current + 'g', b, mid)
	end addString1

	def mkString(start: String, mid: String, end: String): String =
		val sb = new StringBuilder
		sb.append(start)
		this.addString1("", sb, mid)
		sb.length = sb.length - mid.length
		sb.append(end)
		sb.toString
	end mkString

	override def toString: String = this.mkString(s"${scala.io.AnsiColor.RED}Trie${scala.io.AnsiColor.RESET}(", ", ", ")")
end Trie

object Trie:
	final class Node(val value: Long, val w: Trie, val u: Trie, val b: Trie, val r: Trie, val g: Trie) extends Trie:
		override def map(fn: Long => Long): Trie =
			Node(
				fn(this.value),
				this.w.map(fn),
				this.u.map(fn),
				this.b.map(fn),
				this.r.map(fn),
				this.g.map(fn),
			)

		override lazy val isEmpty: Boolean = 0 == value && w.isEmpty && u.isEmpty && b.isEmpty && r.isEmpty && g.isEmpty
	end Node

	private object _empty extends Trie:
		def value: Long = 0
		def w: Trie = _empty
		def u: Trie = _empty
		def b: Trie = _empty
		def r: Trie = _empty
		def g: Trie = _empty

		override def map(fn: Long => Long): Trie = this

		override def addString1(current: String, b: StringBuilder, mid: String): b.type = b
		override def isEmpty: Boolean = true
		override def toString: String = "Trie.empty"
	end _empty

	def empty: Trie = _empty

	def from(elems: IterableOnce[String]): Trie =
		elems.iterator.foldLeft(empty)(_ + _)
	end from
end Trie

val avaliables2 = Trie.from(avaliables)

val part2 = desireds
	.map: desired =>
		def impl(remainingDesired: String, remainingAvaliable: Trie): Long =
			if "" == remainingDesired then
				remainingAvaliable.value
			else
				val head = remainingDesired.charAt(0)
				val tail = remainingDesired.substring(1)

				val nextAvaliable =
						val x = remainingAvaliable.getTrie(head)
						if (0 != x.value) then
							x |+| avaliables2.map(_ * x.value)
						else
							x

				impl(tail, nextAvaliable)
			end if
		impl(desired, avaliables2)

println(s"part 1 v2: ${part2.count(_ != 0)}")
println(s"part 2: ${part2.sum}")
