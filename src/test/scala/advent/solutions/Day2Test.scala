package advent.solutions

import advent.*

class Day2Test
    extends ExampleValidator[2](2, 4)(
      """7 6 4 2 1
        |1 2 7 8 9
        |9 7 6 2 1
        |1 3 2 4 5
        |8 6 4 4 1
        |1 3 6 7 9""".stripMargin,
    ):

  "Day 2, Part 1" should "work for specific example" in {
    val solver = summon[Solver[2, 1]]
    val input = Parser.parse(solver.parser)(Vector("21 25 26 28 31 32 35 38"))

    val solution = solver.solve(input)

    assert(solution == 0)
  }
