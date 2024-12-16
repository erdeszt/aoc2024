package advent

enum Order derives CanEqual:
  case Asc
  case Desc

trait Day2Common:
  type Input = Vector[Vector[Int]]

  val parser: Parser[Input] = PBasic(RArray(Separator.Whitespace, VInt()))

object Day2:

  def isSafe(line: Vector[Int]): Boolean =
    line
      .foldLeft((Option.empty[Order], Option.empty[Int], true)) {
        case ((_, _, false), _)           => (None, None, false)
        case ((None, None, true), number) => (None, Some(number), true)
        case ((Some(_), None, _), _)      => (None, None, false)
        case ((None, Some(previous), true), number) =>
          val diff = Math.abs(previous - number)
          val diffOk = diff >= 1 && diff <= 3
          if (previous == number) {
            (None, None, false)
          } else if (previous < number) {
            (Some(Order.Asc), Some(number), diffOk)
          } else {
            (Some(Order.Desc), Some(number), diffOk)
          }
        case ((Some(order), Some(previous), true), number) =>
          val diff = Math.abs(previous - number)
          val diffOk = diff >= 1 && diff <= 3
          if (order == Order.Asc && previous < number) {
            (Some(Order.Asc), Some(number), diffOk)
          } else if (order == Order.Desc && previous > number) {
            (Some(Order.Desc), Some(number), diffOk)
          } else {
            (None, None, false)
          }
      }
      ._3

  // Slow solution
  def removeSingles(line: Vector[Int]): List[Vector[Int]] =
    line.indices.map { index =>
      line.take(index) ++ line.drop(index + 1)
    }.toList

given day2part1Solution: Solver[2, 1] = new Solver[2, 1] with Day2Common:

  override def solve(input: Vector[Vector[Int]]): Int =
    input.foldLeft(0) { case (safe, line) =>
      if Day2.isSafe(line) then safe + 1
      else safe
    }

given day2part2Solution: Solver[2, 2] = new Solver[2, 2] with Day2Common:

  override def solve(input: Vector[Vector[Int]]): Int =
    input.foldLeft(0) { case (safe, line) =>
      if (Day2.isSafe(line) || Day2.removeSingles(line).exists(Day2.isSafe)) {
        safe + 1
      } else { safe }
    }
