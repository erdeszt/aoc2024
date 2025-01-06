package advent.solutions

import advent.*

trait Day2Common:
  type Input = Vector[Vector[Long]]

  val parser: Parser[Input] = PBasic(RArray(Separator.Whitespace, VNum()))

  enum Order derives CanEqual:
    case Asc
    case Desc

  def isSafe(line: Vector[Long]): Boolean =
    line
      .foldLeft((Option.empty[Order], Option.empty[Long], true)) {
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
  def removeSingles(line: Vector[Long]): List[Vector[Long]] =
    line.indices.map { index =>
      line.take(index) ++ line.drop(index + 1)
    }.toList

given day2part1Solution: Solver[2, 1] = new Solver[2, 1] with Day2Common:

  override def solve(input: Vector[Vector[Long]]): Long =
    input.foldLeft(0) { case (safe, line) =>
      if isSafe(line) then safe + 1
      else safe
    }

given day2part2Solution: Solver[2, 2] = new Solver[2, 2] with Day2Common:

  override def solve(input: Vector[Vector[Long]]): Long =
    input.foldLeft(0) { case (safe, line) =>
      if (isSafe(line) || removeSingles(line).exists(isSafe)) {
        safe + 1
      } else { safe }
    }
