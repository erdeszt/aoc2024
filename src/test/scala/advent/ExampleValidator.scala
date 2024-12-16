package advent

import org.scalatest.flatspec.AnyFlatSpec

trait ExampleValidator[Day <: Int: ValueOf](
    part1Solution: Int,
    part2Solution: Int,
)(examples: String | (String, String))(using
    Solver[Day, 1],
    Solver[Day, 2],
) extends AnyFlatSpec:

  private type Selector = ((String, String)) => String

  private def validateExample[Part <: Int: ValueOf](using
      solver: Solver[Day, Part],
  ): Unit =
    s"Day ${valueOf[Day]}, Part ${valueOf[Part]}" should "produce the correct result for the example" in {
      val (selector, expectedResult): (Selector, Int) =
        valueOf[Part] match
          case 1 => ((_._1): Selector, part1Solution)
          case 2 => ((_._2): Selector, part2Solution)
          case _ => throw Exception(s"Invalid part number: ${valueOf[Part]}")
      val rawInput = examples match
        case tuple: (String, String) => selector(tuple)
        case vector: String          => vector
      val input = Parser.parse(solver.parser)(rawInput.split("\n").toVector)

      val solution = solver.solve(input)

      assert(solution == expectedResult)
    }

  validateExample[1]
  validateExample[2]
