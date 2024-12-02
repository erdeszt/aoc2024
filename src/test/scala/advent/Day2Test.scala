package advent

import org.scalatest.flatspec.AnyFlatSpec

class Day2Test extends AnyFlatSpec:

  val day2Example: Vector[String] = Vector(
    "7 6 4 2 1",
    "1 2 7 8 9",
    "9 7 6 2 1",
    "1 3 2 4 5",
    "8 6 4 4 1",
    "1 3 6 7 9",
  )

  "Day 2, Part 1" should "produce the correct result for the example input" in {
    val solution = summon[Solver[2, 1]].solve(day2Example)

    assert(solution == 2)
  }

  "Day 2, Part 1" should "work for specific example" in {
    val solution = summon[Solver[2, 1]].solve(Vector("21 25 26 28 31 32 35 38"))

    assert(solution == 0)
  }

  "Day 2, Part 2" should "produce the correct result for the example input" in {
    val solution = summon[Solver[2, 2]].solve(day2Example)

    assert(solution == 4)
  }
