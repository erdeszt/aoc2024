package advent

import org.scalatest.flatspec.AnyFlatSpec

trait ExampleValidator[Day <: Int: ValueOf](
    part1Solution: Int,
    part2Solution: Int,
)(examples: Vector[String] | (Vector[String], Vector[String]))(using
    Solver[Day, 1],
    Solver[Day, 2],
) extends AnyFlatSpec:

  private type Selector = ((Vector[String], Vector[String])) => Vector[String]

  private def validateExample[Part <: Int: ValueOf](using
      Solver[Day, Part],
  ): Unit = {
    s"Day ${valueOf[Day]}, Part ${valueOf[Part]}" should "produce the correct result for the example" in {
      val (selector, expectedResult): (Selector, Int) =
        valueOf[Part] match
          case 1 => ((_._1): Selector, part1Solution)
          case 2 => ((_._2): Selector, part2Solution)
          case _ => throw Exception(s"Invalid part number: ${valueOf[Part]}")

      val solution = summon[Solver[Day, Part]].solve(
        examples match
          case tuple: (Vector[_], Vector[_]) => selector(tuple)
          case vector: Vector[_]             => vector,
      )

      assert(solution == expectedResult)
    }

  }

  validateExample[1]
  validateExample[2]
