package advent.solutions

import advent.*

trait Day9Common:
  type Input = Vector[Char]

  val parser: Parser[Input] = PSingle(RChars())

given day9part1Solution: Solver[9, 1] = new Solver[9, 1] with Day9Common:
  override def solve(input: Vector[Char]): Long =
    ???

given day9part2Solution: Solver[9, 2] = new Solver[9, 2] with Day9Common:
  override def solve(input: Vector[Char]): Long =
    ???
