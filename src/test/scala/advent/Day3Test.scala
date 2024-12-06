package advent

import org.scalatest.flatspec.AnyFlatSpec

class Day3Test
    extends ExampleTest[3](161, 48)(
      (
        Vector(
          "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))",
        ),
        Vector(
          "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))",
        ),
      ),
    )
