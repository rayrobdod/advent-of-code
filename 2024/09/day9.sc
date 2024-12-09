//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using dep io.github.java-diff-utils:java-diff-utils:4.15

import scala.annotation.nowarn
import scala.annotation.tailrec

extension [A](self: Vector[A])
	def removed(index: Int): Vector[A] =
		val (prefix, suffix) = self.splitAt(index)
		prefix :++ suffix.tail
	def inserted(index: Int, value: A): Vector[A] =
		val (prefix, suffix) = self.splitAt(index)
		prefix :+ value :++ suffix

final case class DiskMapEntry(fileLength: Int, fileId: Int, freeLength: Int)

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

val input: Vector[DiskMapEntry] =
	os.read(os.pwd / "input.txt")
		.filter(_.isDigit)
		.map(_ & 0x0F)
		.appended(0)
		.grouped(2)
		.zipWithIndex
		.map({case (Seq(a,b), index) => DiskMapEntry(a, index, b)})
		.toVector

def filesystemSize(diskMap: Vector[DiskMapEntry]): Int =
	diskMap.map{(e) => e.fileLength + e.freeLength}.sum

val inputFilesystemSize = filesystemSize(input)
println(s"filesystem size: $inputFilesystemSize")


locally:
	@tailrec def compactedChecksum(
		diskMap: Vector[DiskMapEntry],
		block: Int,
		checksum: Long,
	): Long =
		//println("ENTERING: checksum(" + diskMap + ", " + block + ", " + checksum + ")")
		//new java.util.Scanner(System.in).nextLine()

		diskMap: @nowarn match
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
	val part1 = compactedChecksum(input, 0, 0)

	println(s"part 1: ${part1}")

locally:
	def showFilesystemSmall(fs: Vector[DiskMapEntry]):String =
		fs.map:
			case DiskMapEntry(len, id, free) =>
				id.toString * len + "." * free
		.mkString

	def showFilesystemLarge(fs: Vector[DiskMapEntry]):String =
		import scala.util.chaining.scalaUtilChainingOps
		fs
			.map:
				case DiskMapEntry(len, id, free) =>
					f"($len,$id%04d,$free)"
			.pipe: x =>
				(x.take(10) :+ " ... ") ++: x.takeRight(10)
			.mkString(", ")

	def showFilesystemTake(fs: Vector[DiskMapEntry]):String =
		import scala.util.chaining.scalaUtilChainingOps
		fs
			.map:
				case DiskMapEntry(len, id, free) =>
					f"($len,$id%04d,$free)"
			.pipe: x =>
				if x.sizeIs < 4 then
					x
				else
					(x.take(4) :+ " ... ")
			.mkString(", ")

	def compact(initialFilesystem: Vector[DiskMapEntry]): Vector[DiskMapEntry] =
		@tailrec def impl(
			diskMap: Vector[DiskMapEntry],
			fileId: Int,
		): Vector[DiskMapEntry] =
			//println(s"ENTERING: compact.impl(${showFilesystemSmall(diskMap)}, $fileId)")
			//new java.util.Scanner(System.in).nextLine()

			if fileId <= 0 then
				diskMap
			else
				val fileIndex = diskMap.indexWhere(_.fileId == fileId)
				val file = diskMap(fileIndex)

				val freeIndex = diskMap.indexWhere(_.freeLength >= file.fileLength)

				val newDiskMap =
					if freeIndex < 0 || freeIndex >= fileIndex then
						diskMap

					else if freeIndex + 1 == fileIndex then
						val free = diskMap(freeIndex)
						val newFree = free.copy(freeLength = 0)

						val newFile = file.copy(freeLength = free.freeLength + file.freeLength)

						diskMap
							.updated(freeIndex, newFree)
							.updated(fileIndex, newFile)

					else
						val prior = diskMap(fileIndex - 1)
						val newPrior = prior.copy(freeLength = prior.freeLength + file.fileLength + file.freeLength)

						val free = diskMap(freeIndex)
						val newFree = free.copy(freeLength = 0)

						val newFile = file.copy(freeLength = free.freeLength - file.fileLength)

						diskMap
							.updated(fileIndex - 1, newPrior)
							.removed(fileIndex)
							.updated(freeIndex, newFree)
							.inserted(freeIndex + 1, newFile)
					end if

				if inputFilesystemSize != filesystemSize(newDiskMap) then
					import scala.jdk.CollectionConverters.given
					import com.github.difflib.DiffUtils
					val msg = DiffUtils.diff(diskMap.asJava, newDiskMap.asJava).toString
					throw new Exception(msg)
				end if

				impl(newDiskMap, fileId - 1)
			end if
		end impl

		impl(initialFilesystem, initialFilesystem.last.fileId)
	end compact

	def diskChecksum(
		diskMap: Vector[DiskMapEntry],
	): Long =
		@tailrec def impl(
			diskMap: Vector[DiskMapEntry],
			block: Int,
			checksum: Long,
		): Long =
			//println(s"ENTERING: checksum.impl(${showFilesystemTake(diskMap)}, $block, $checksum)")
			diskMap: @nowarn match
				case DiskMapEmpty() => checksum
				case DiskMapHeadTail(DiskMapEntry(len, id, free), tail) =>
					impl(
						tail,
						block + len + free,
						checksum + (block until (block + len)).sum.toLong * id,
					)
		end impl

		impl(diskMap, 0, 0L)

	val part2 = diskChecksum(compact(input))

	println(s"part 2: ${part2}")
