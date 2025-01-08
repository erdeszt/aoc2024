package advent.solutions

import advent.*

trait Day8Common:
  type Input = Vector[Vector[Char]]

  val parser: Parser[Input] = PBasic(RChars())

given day8part1Solution: Solver[8, 1] = new Solver[8, 1] with Day8Common:

  override def solve(input: Vector[Vector[Char]]): Long =
    assert(input.nonEmpty)

    val height = input.length
    val width = input.head.length
    val antennaIndices =
      scala.collection.mutable.HashMap.empty[Char, Vector[Pos]]

    for ((row, rowIdx) <- input.zipWithIndex) {
      for ((char, colIdx) <- row.zipWithIndex) {
        if (char != '.') {
          val _ = antennaIndices.updateWith(char) {
            case None            => Some(Vector(Pos(rowIdx, colIdx)))
            case Some(positions) => Some(positions :+ Pos(rowIdx, colIdx))
          }
        }
      }
    }

    val interferencePoints = scala.collection.mutable.HashSet.empty[Pos]

    for ((antennaCode, positions) <- antennaIndices) {
      for (basePosition <- positions) {
        for (targetPosition <- positions) {
          if (basePosition != targetPosition) {
            val diff = targetPosition - basePosition

            val pos1 = basePosition - diff
            val pos2 = targetPosition + diff

            if (pos1.isInBox(width, height)) {
              val _ = interferencePoints.add(pos1)
            }

            if (pos2.isInBox(width, height)) {
              val _ = interferencePoints.add(pos2)
            }
          }
        }
      }
    }

    interferencePoints.size

given day8part2Solution: Solver[8, 2] = new Solver[8, 2] with Day8Common:
  override def solve(input: Vector[Vector[Char]]): Long =
    ???
