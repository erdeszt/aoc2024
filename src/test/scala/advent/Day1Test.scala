package advent

import org.scalatest.flatspec.AnyFlatSpec

class Day1Test
    extends ExampleTest[1](11, 31)(
      Vector(
        "3   4",
        "4   3",
        "2   5",
        "1   3",
        "3   9",
        "3   3",
      ),
    )
