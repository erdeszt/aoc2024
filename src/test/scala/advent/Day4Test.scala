package advent

import org.scalatest.flatspec.AnyFlatSpec

class Day4Test extends AnyFlatSpec:

  val day4Example: Vector[String] = Vector(
    "MMMSXXMASM",
    "MSAMXMSMSA",
    "AMXSXMAAMM",
    "MSAMASMSMX",
    "XMASAMXAMM",
    "XXAMMXXAMA",
    "SMSMSASXSS",
    "SAXAMASAAA",
    "MAMMMXMMMM",
    "MXMXAXMASX",
  )

  "Day4, Part 1" should "produce the correct output for the example input" in {
    val solution = summon[Solver[4, 1]].solve(day4Example)

    assert(solution == 18)
  }

  "Day4, Part 2" should "produce the correct output for the example input" in {
    val solution = summon[Solver[4, 2]].solve(day4Example)

    assert(solution == 9)
  }
