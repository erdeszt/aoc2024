package advent

import org.scalatest.flatspec.AnyFlatSpec

class Day3Test extends AnyFlatSpec:

  val day3Part1Example: Vector[String] = Vector("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
  val day3Part2Example: Vector[String] = Vector("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

  "Day 3, Part 1" should "produce the correct result for the example input" in {
    val solution = summon[Solver[3, 1]].solve(day3Part1Example)

    assert(solution == 161)
  }

  "Day 3, Part 2" should "produce the correct result for the example input" in {
    val solution = summon[Solver[3, 2]].solve(day3Part2Example)

    assert(solution == 48)
  }

end Day3Test

