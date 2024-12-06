package advent

class Day2Test
    extends ExampleValidator[2](2, 4)(
      Vector(
        "7 6 4 2 1",
        "1 2 7 8 9",
        "9 7 6 2 1",
        "1 3 2 4 5",
        "8 6 4 4 1",
        "1 3 6 7 9",
      ),
    ):

  "Day 2, Part 1" should "work for specific example" in {
    val solution = summon[Solver[2, 1]].solve(Vector("21 25 26 28 31 32 35 38"))

    assert(solution == 0)
  }
