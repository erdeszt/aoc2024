package advent.solutions

import advent.*

trait Day1Common:
  type Input = Vector[(Long, Long)]

  val parser: Parser[Input] = PBasic(
    RTuple(Separator.Whitespace, VNum(), VNum()),
  )

given day1Part1Solution: Solver[1, 1] = new Solver[1, 1] with Day1Common:

  override def solve(input: Vector[(Long, Long)]): Long =
    val (left, right) = input.unzip

    left.sorted.zip(right.sorted).map(_ - _).map(Math.abs).sum

given day1Part2Solution: Solver[1, 2] = new Solver[1, 2] with Day1Common:

  override def solve(input: Vector[(Long, Long)]): Long =
    val (frequencies, numbers) = input
      .foldLeft((Map.empty[Long, Long], Vector.empty[Long])) {
        case ((frequencies, numbers), (left, right)) =>
          (
            frequencies.updatedWith(right) {
              case None          => Some(1)
              case Some(current) => Some(current + 1)
            },
            numbers :+ left,
          )
      }

    numbers.map(number => frequencies.getOrElse(number, 0L) * number).sum
