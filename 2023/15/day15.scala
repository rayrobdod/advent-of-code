//> using scala 3.3.1
//> using dep com.lihaoyi::os-lib:0.9.2

import scala.annotation.tailrec

def hash(s:String): Int =
	s.foldLeft(0): (folding, c) =>
		((folding + c) * 17) % 256

type LabelLensPair = (String, Int)

def arrangementProcedure(folding: Seq[Seq[LabelLensPair]], instruction: String): Seq[Seq[LabelLensPair]] =
	instruction match
		case s"${label}-" =>
			val boxIndex = hash(label)
			val box = folding(boxIndex)
			val updatedBox = box.filter(_._1 != label)
			folding.updated(boxIndex, updatedBox)

		case s"${label}=${number}" =>
			val boxIndex = hash(label)
			val box = folding(boxIndex)

			val lensIndex = box.indexWhere(_._1 == label)
			val updatedBox =
				if lensIndex < 0 then
					box :+ ((label, number.toInt))
				else
					box.updated(lensIndex, ((label, number.toInt)))

			folding.updated(boxIndex, updatedBox)

def focusingPower(data: Seq[Seq[LabelLensPair]]) =
	data.zipWithIndex
		.map: (box, boxIndex) =>
			box.zipWithIndex
				.map[Int]: (lens, lensIndex) =>
					val (_, focalLength) = lens
					(boxIndex + 1) * (lensIndex + 1) * focalLength
				.sum
		.sum

object Day15:
	def main(args:Array[String]):Unit =
		val input =
			os.read(os.pwd / "input.txt")
				.split(",").toList

		System.out.print("part 1: ")
		System.out.println:
			input.map(hash).sum

		System.out.print("part 2: ")
		System.out.println:
			val emptyBoxes: Seq[Seq[LabelLensPair]] = Seq.fill(256)(Seq.empty)

			focusingPower:
				input.foldLeft(emptyBoxes):
					arrangementProcedure
