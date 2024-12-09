//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3

import scala.annotation.tailrec

final case class DiskMapEntry(fileLength: Int, fileIndex: Int, freeLength: Int)

object DiskMapEmpty:
	def unapply(v: Vector[DiskMapEntry]): Boolean =
		v match
			case Vector() => true
			case Vector(DiskMapEntry(0, _, _)) => true
			case _ => false

object DiskMapHeadMidLast:
	def unapply(v: Vector[DiskMapEntry]): Option[(DiskMapEntry, Vector[DiskMapEntry], DiskMapEntry)] =
		if v.length < 2 then
			None
		else
			val h = v.head
			val mid = v.tail.init
			val last = v.last

			Some(h, mid, last)

object DiskMapHeadTail:
	def unapply(v: Vector[DiskMapEntry]): Option[(DiskMapEntry, Vector[DiskMapEntry])] =
		if v.length < 1 then
			None
		else
			val h = v.head
			val tail = v.tail

			Some(h, tail)

val filesystem: Vector[DiskMapEntry] =
	os.read(os.pwd / "input.txt")
		.filter(_.isDigit)
		.map(_ & 0x0F)
		.appended(0)
		.grouped(2)
		.zipWithIndex
		.map({case (Seq(a,b), index) => DiskMapEntry(a, index, b)})
		.toVector

//val disk_size: Int = input_raw.map(_ & 0x0F).sum
//val disk = new Array[Byte](disk_size)

locally:
	@tailrec def compactedChecksum(
		filesystem: Vector[DiskMapEntry],
		block: Int,
		checksum: Long,
	): Long =
		//println("ENTERING: checksum(" + filesystem + ", " + block + ", " + checksum + ")")
		//new java.util.Scanner(System.in).nextLine()

		filesystem match
			case DiskMapEmpty() => checksum
			case DiskMapHeadMidLast(DiskMapEntry(0, _, 1), mid, DiskMapEntry(1, fileId, _)) =>
				compactedChecksum(
					mid,
					block + 1,
					checksum + (block * fileId),
				)
			case DiskMapHeadMidLast(DiskMapEntry(0, _, free), mid, DiskMapEntry(1, fileId, _)) =>
				compactedChecksum(
					DiskMapEntry(0, 0, free - 1) +: mid,
					block + 1,
					checksum + (block * fileId),
				)
			case DiskMapHeadMidLast(DiskMapEntry(0, _, 1), mid, DiskMapEntry(lastLength, fileId, _)) =>
				compactedChecksum(
					mid :+ DiskMapEntry(lastLength - 1, fileId, 0),
					block + 1,
					checksum + (block * fileId),
				)
			case DiskMapHeadMidLast(DiskMapEntry(0, _, free), mid, DiskMapEntry(lastLength, fileId, _)) =>
				compactedChecksum(
					DiskMapEntry(0, 0, free - 1) +: mid :+ DiskMapEntry(lastLength - 1, fileId, 0),
					block + 1,
					checksum + (block * fileId),
				)
			case DiskMapHeadTail(DiskMapEntry(1, fileId, 0), tail) =>
				compactedChecksum(
					tail,
					block + 1,
					checksum + (block * fileId),
				)
			case DiskMapHeadTail(DiskMapEntry(len, fileId, free), tail) =>
				compactedChecksum(
					DiskMapEntry(len - 1, fileId, free) +: tail,
					block + 1,
					checksum + (block * fileId),
				)
	val part1 = compactedChecksum(filesystem, 0, 0)

	println(s"part 1: ${part1}")

locally:
	val part2 = null

	println(s"part 2: ${part2}")
