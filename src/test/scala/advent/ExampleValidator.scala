package advent

import org.scalatest.flatspec.AnyFlatSpec

trait ExampleValidator[Day <: Int: ValueOf](
    part1Solution: Int,
    part2Solution: Int,
)(examples: Vector[String] | (Vector[String], Vector[String]))(using
    Solver[Day, 1],
    Solver[Day, 2],
) extends AnyFlatSpec:

  s"Day ${valueOf[Day]}, Part 1" should "produce the correct result for the example" in {
    val solution = summon[Solver[Day, 1]].solve(
      examples match
        case (part1Example, part2Example) => part1Example
        case example: Vector[_]           => example,
    )

    assert(solution == part1Solution)
  }

  s"Day ${valueOf[Day]}, Part 2" should "produce the correct result for the example" in {
    val solution = summon[Solver[Day, 2]].solve(
      examples match
        case (part1Example, part2Example) => part2Example
        case example: Vector[_]           => example,
    )

    assert(solution == part2Solution)
  }
