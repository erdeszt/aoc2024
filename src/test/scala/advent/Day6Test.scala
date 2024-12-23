package advent

class Day6Test
    extends ExampleValidator[6](
      41,
      -1,
    )(
      """....#.....
        |.........#
        |..........
        |..#.......
        |.......#..
        |..........
        |.#..^.....
        |........#.
        |#.........
        |......#...""".stripMargin,
    )
