package advent

trait Day3Common:
  type Input = Vector[String]

  val parser: Parser[Vector[String]] = PBasic(RString())

  def extractMulResults(input: String): Long =
    val mulPattern = "mul\\((\\d+),(\\d+)\\)".r

    val matches = mulPattern.findAllMatchIn(input)

    matches.map(m => m.group(1).toInt * m.group(2).toInt).sum

given day3part1Solution: Solver[3, 1] = new Solver[3, 1] with Day3Common:
  override def solve(input: Vector[String]): Long =
    input.map(extractMulResults).sum

given day3part2Solution: Solver[3, 2] = new Solver[3, 2] with Day3Common:
  override def solve(input: Vector[String]): Long =
    input
      .foldLeft((0L, true)) { case ((sum, isEnabled), line) =>
        line.split("don't\\(\\)") match
          case Array() => throw InvalidLineError(line)
          case Array(single) =>
            if isEnabled then (sum + extractMulResults(single), isEnabled)
            else (sum, isEnabled)
          case Array(first, others*) =>
            val updatedSum = first.split("do\\(\\)") match
              case Array() => throw InvalidLineError(line)
              case Array(single) =>
                if isEnabled then sum + extractMulResults(single) else sum
              case Array(ffirst, parts*) =>
                val ssum =
                  if isEnabled then sum + extractMulResults(ffirst) else sum
                ssum + parts.map(extractMulResults).sum

            val finalSum = others.foldLeft(updatedSum) {
              case (currentSum, part) =>
                part.split("do\\(\\)") match
                  case Array() => throw InvalidLineError(line)
                  case Array(single) =>
                    currentSum
                  case Array(_, parts*) =>
                    currentSum + parts.map(extractMulResults).sum
            }

            (finalSum, others.last.contains("do()"))
      }
      ._1
