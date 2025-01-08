package advent.solutions

import advent.*

trait Day8Common:
  type Input = Vector[Vector[Char]]

  val parser: Parser[Input] = PBasic(RChars())

  def solveImpl(
      input: Vector[Vector[Char]],
      maxSteps: Long,
      addAntennas: Boolean,
  ): Long =
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

            var step = 0
            var pos = basePosition - diff
            while (pos.isInBox(width, height) && step < maxSteps) {
              val _ = interferencePoints.add(pos)
              pos = pos - diff
              step += 1
            }

            pos = targetPosition + diff
            step = 0
            while (pos.isInBox(width, height) && step < maxSteps) {
              val _ = interferencePoints.add(pos)
              pos = pos + diff
              step += 1
            }

            if (addAntennas) {
              val _ = interferencePoints.add(basePosition)
              val _ = interferencePoints.add(targetPosition)
            }
          }
        }
      }
    }

    interferencePoints.size

given day8part1Solution: Solver[8, 1] = new Solver[8, 1] with Day8Common:

  override def solve(input: Vector[Vector[Char]]): Long =
    solveImpl(input, maxSteps = 1, addAntennas = false)

given day8part2Solution: Solver[8, 2] = new Solver[8, 2] with Day8Common:
  override def solve(input: Vector[Vector[Char]]): Long =
    solveImpl(input, maxSteps = Long.MaxValue, addAntennas = true)
