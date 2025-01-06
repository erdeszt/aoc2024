package advent.solutions

import advent.*

trait Day8Common:
  type Input = Vector[Vector[Char]]

  val parser: Parser[Input] = PBasic(RChars())

given day8part1Solution: Solver[8, 1] = new Solver[8, 1] with Day8Common:

  override def solve(input: Vector[Vector[Char]]): Long =
    ???

given day8part2Solution: Solver[8, 2] = new Solver[8, 2] with Day8Common:
  override def solve(input: Vector[Vector[Char]]): Long =
    ???
