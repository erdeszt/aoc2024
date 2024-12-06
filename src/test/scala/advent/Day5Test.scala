package advent

import org.scalatest.flatspec.AnyFlatSpec

class Day5Test extends AnyFlatSpec:

  val day5Example: Vector[String] = Vector(
    "47|53",
    "97|13",
    "97|61",
    "97|47",
    "75|29",
    "61|13",
    "75|53",
    "29|13",
    "97|29",
    "53|29",
    "61|53",
    "97|53",
    "61|29",
    "47|13",
    "75|47",
    "97|75",
    "47|61",
    "75|61",
    "47|29",
    "75|13",
    "53|13",
    "",
    "75,47,61,53,29",
    "97,61,53,29,13",
    "75,29,13",
    "75,97,47,61,53",
    "61,13,29",
    "97,13,75,29,47",
  )

  "Day5, Part 1" should "produce the correct output for the example input" in {
    val solution = summon[Solver[5, 1]].solve(day5Example)

    assert(solution == 143)
  }

  "Day5, Part 2" should "produce the correct output for the example input" in {
    val solution = summon[Solver[5, 2]].solve(day5Example)

    assert(solution == 123)
  }
