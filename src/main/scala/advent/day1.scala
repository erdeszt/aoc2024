package advent

trait Day1Common:
  type Input = Vector[(Int, Int)]

  val parser: Parser[Input] = PBasic(
    RTuple(Separator.Whitespace, VInt(), VInt()),
  )

given day1Part1Solution: Solver[1, 1] = new Solver[1, 1] with Day1Common:

  override def solve(input: Vector[(Int, Int)]): Int =
    val (left, right) = input.unzip

    left.sorted.zip(right.sorted).map(_ - _).map(Math.abs).sum

given day1Part2Solution: Solver[1, 2] = new Solver[1, 2] with Day1Common:

  override def solve(input: Vector[(Int, Int)]): Int =
    val (frequencies, numbers) = input
      .foldLeft((Map.empty[Int, Int], Vector.empty[Int])) {
        case ((frequencies, numbers), (left, right)) =>
          (
            frequencies.updatedWith(right) {
              case None          => Some(1)
              case Some(current) => Some(current + 1)
            },
            numbers :+ left,
          )
      }

    numbers.map(number => frequencies.getOrElse(number, 0) * number).sum
