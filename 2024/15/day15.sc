//> using scala 3.5.2
//> using dep com.lihaoyi::os-lib:0.11.3
//> using file ../../2023/Grid.scala

import scala.annotation.tailrec
import name.rayrobdod.aoc.*

val inputFile = os.pwd / "input.txt"

enum WarehouseCell:
	case Wall, Box, Open, Robot

	def show: Char =
		this match
			case Wall => '#'
			case Box => 'O'
			case Open => '.'
			case Robot => '@'

val (warehouse: Grid[WarehouseCell], moves: Seq[Direction]) = locally:
	val inputLines = os.read.lines(inputFile)
	val splitLine = inputLines.indexOf("")
	val charGrid = Grid.fromStrings(inputLines.take(splitLine))
	val warehouse = charGrid.map:
		case '#' => WarehouseCell.Wall
		case 'O' => WarehouseCell.Box
		case '.' => WarehouseCell.Open
		case '@' => WarehouseCell.Robot

	val moves = inputLines.drop(splitLine).flatMap:
		_.collect:
			case '<' => Direction.Left
			case '>' => Direction.Right
			case '^' => Direction.Up
			case 'v' => Direction.Down
	(warehouse, moves)

locally:
	def applyRobotMove(warehouse: Grid[WarehouseCell], d: Direction): Grid[WarehouseCell] =
		val robotPosition = warehouse.indexOf(WarehouseCell.Robot)
		def endOfBoxSequence(p: Point): Point =
			val nextP = p + d.toUnitVector
			warehouse(nextP) match
				case WarehouseCell.Wall | WarehouseCell.Open => nextP
				case _ => endOfBoxSequence(nextP)
		val wallOrOpenPosition = endOfBoxSequence(robotPosition)
		warehouse(wallOrOpenPosition) match
			case WarehouseCell.Wall => warehouse
			case WarehouseCell.Open =>
				if robotPosition + d.toUnitVector == wallOrOpenPosition then
					warehouse
						.updated(robotPosition, WarehouseCell.Open)
						.updated(robotPosition + d.toUnitVector, WarehouseCell.Robot)
				else
					warehouse
						.updated(robotPosition, WarehouseCell.Open)
						.updated(robotPosition + d.toUnitVector, WarehouseCell.Robot)
						.updated(wallOrOpenPosition, WarehouseCell.Box)
			case _ => ???
	end applyRobotMove

	/*
	moves.foldLeft(warehouse): (folding, move) =>
		val nextFolding = applyRobotMove(folding, move)
		println(nextFolding.map(_.show).mkString)
		println()
		nextFolding
	*/

	val warehouseEndState = moves.foldLeft(warehouse)(applyRobotMove)
	/*
	println(warehouseEndState.map(_.show).mkString)
	*/
	val part1 = warehouseEndState
		.indicesWhere:
			_ == WarehouseCell.Box
		.map: p =>
			p.x + p.y * 100
		.sum

	println(s"part 1: $part1")

locally:
	enum WideWarehouseCell:
		case Wall, BoxLeft, BoxRight, Open, Robot

		def show: Char =
			this match
				case Wall => '#'
				case Open => '.'
				case Robot => '@'
				case BoxLeft => '['
				case BoxRight => ']'

	val wideWarehouse: Grid[WideWarehouseCell] =
		Grid.tabulate(warehouse.width * 2, warehouse.height): p =>
			warehouse(Point(p.x / 2, p.y)) match
				case WarehouseCell.Wall =>
					WideWarehouseCell.Wall
				case WarehouseCell.Open =>
					WideWarehouseCell.Open
				case WarehouseCell.Robot =>
					if 0 == p.x % 2 then
						WideWarehouseCell.Robot
					else
						WideWarehouseCell.Open
				case WarehouseCell.Box =>
					if 0 == p.x % 2 then
						WideWarehouseCell.BoxLeft
					else
						WideWarehouseCell.BoxRight
	end wideWarehouse

	def applyRobotMove(warehouse: Grid[WideWarehouseCell], d: Direction): Grid[WideWarehouseCell] =
		val robotPosition = warehouse.indexOf(WideWarehouseCell.Robot)
		d match
			case Direction.Left | Direction.Right =>
				def doMove(p: Point): (Grid[WideWarehouseCell], Boolean) =
					warehouse(p) match
						case WideWarehouseCell.Wall => (warehouse, false)
						case WideWarehouseCell.Open => (warehouse, true)
						case _ =>
							val (folding, shouldMove) = doMove(p + d.toUnitVector)
							if shouldMove then
								(folding.updated(p + d.toUnitVector, folding(p)), true)
							else
								(folding, false)
				end doMove
				val (folding, shouldMove) = doMove(robotPosition)
				if shouldMove then
					folding.updated(robotPosition, WideWarehouseCell.Open)
				else
					folding
			case _ =>
				def canMoveInto(p: Point): Boolean =
					warehouse(p) match
						case WideWarehouseCell.Wall => false
						case WideWarehouseCell.Open => true
						case WideWarehouseCell.Robot => canMoveInto(p + d.toUnitVector)
						case WideWarehouseCell.BoxLeft => canMoveInto(p + d.toUnitVector) && canMoveInto(p + d.toUnitVector + Direction.Right.toUnitVector)
						case WideWarehouseCell.BoxRight => canMoveInto(p + d.toUnitVector) && canMoveInto(p + d.toUnitVector + Direction.Left.toUnitVector)

				def cellsToMove(p: Point): Set[Point] =
					warehouse(p) match
						case WideWarehouseCell.Wall => ???
						case WideWarehouseCell.Open => Set.empty
						case WideWarehouseCell.Robot => cellsToMove(p + d.toUnitVector) + p
						case WideWarehouseCell.BoxLeft => cellsToMove(p + d.toUnitVector) ++ cellsToMove(p + d.toUnitVector + Direction.Right.toUnitVector) + p + (p + Direction.Right.toUnitVector)
						case WideWarehouseCell.BoxRight => cellsToMove(p + d.toUnitVector) ++ cellsToMove(p + d.toUnitVector + Direction.Left.toUnitVector) + p + (p + Direction.Left.toUnitVector)

				val canMove = canMoveInto(robotPosition)
				if canMove then
					val toMove = cellsToMove(robotPosition)
					val toClear = toMove.filterNot: p =>
						toMove.contains(p - d.toUnitVector)

					toClear.foldLeft(
						toMove.foldLeft(warehouse): (folding, p) =>
							folding.updated(p + d.toUnitVector, warehouse(p))
						): (folding, p) =>
							folding.updated(p, WideWarehouseCell.Open)
				else
					warehouse
	end applyRobotMove

	//println(wideWarehouse.map(_.show).mkString)
	//println()
	val warehouseEndState = moves.foldLeft(wideWarehouse): (folding, move) =>
		//println(s"Move: $move")
		val nextFolding = applyRobotMove(folding, move)
		//println(nextFolding.map(_.show).mkString)
		//println()
		nextFolding

	val part2 = warehouseEndState
		.indicesWhere:
			_ == WideWarehouseCell.BoxLeft
		.map: p =>
			p.x + p.y * 100
		.sum

	println(s"part 2: $part2")
