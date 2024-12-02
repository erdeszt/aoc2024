package advent

import org.scalatest.flatspec.AnyFlatSpec

class Day1Test extends AnyFlatSpec:

  val day1Example: Vector[String] = Vector(
    "3   4",
    "4   3",
    "2   5",
    "1   3",
    "3   9",
    "3   3",
  )

  "Day 1, Part 1" should "produce the correct result for the example" in {
    val solution = summon[Solver[1, 1]].solve(day1Example)

    assert(solution == 11)
  }

  "Day 1, Part 2" should "produce the correct result for the example" in {
    val solution = summon[Solver[1, 2]].solve(day1Example)

    assert(solution == 31)
  }
