package advent

given day1Part1Solution: Solver[1, 1] = new Solver[1, 1]:

  override def solve(input: Vector[String]): Int =
    val (left, right) =
      input.foldLeft((Vector.empty[Int], Vector.empty[Int])) {
        case ((lefts, rights), line) =>
          line.split("\\s+") match
            case Array(currentLeft, currentRight) =>
              (lefts :+ currentLeft.toInt, rights :+ currentRight.toInt)
            case _ => throw InvalidLineError(line)
      }

    left.sorted.zip(right.sorted).map(_ - _).map(Math.abs).sum

given day1Part2Solution: Solver[1, 2] = new Solver[1, 2]:

  override def solve(input: Vector[String]): Int =
    val (frequencies, numbers) = input
      .foldLeft((Map.empty[Int, Int], Vector.empty[Int])) {
        case ((frequencies, numbers), line) =>
          line.split("\\s+") match
            case Array(left, right) =>
              (
                frequencies.updatedWith(right.toInt) {
                  case None          => Some(1)
                  case Some(current) => Some(current + 1)
                },
                numbers :+ left.toInt,
              )
            case _ => throw InvalidLineError(line)
      }

    numbers.map(number => frequencies.getOrElse(number, 0) * number).sum
