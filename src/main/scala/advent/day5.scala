package advent

trait Day5Common:

  type Input = (Vector[(Int, Int)], Vector[Vector[Int]])

  val parser: Parser[Input] =
    PThen(
      PBasic(RTuple(Separator.Pipe, VInt(), VInt())),
      PBasic(RArray(Separator.Comma, VInt())),
    )

  def toMultiMap(rules: Vector[(Int, Int)]): Map[Int, Set[Int]] =
    rules.foldLeft(Map.empty[Int, Set[Int]]) { case (map, (before, after)) =>
      map.updatedWith(before) {
        case None      => Some(Set(after))
        case Some(set) => Some(set + after)
      }
    }

  def isPageValid(page: Vector[Int], rules: Map[Int, Set[Int]]): Boolean =
    var valid = true

    for (idx <- 1.until(page.length)) {
      rules.get(page(idx)) match
        case None => ()
        case Some(afters) =>
          for (jdx <- 0.until(idx)) {
            if (afters.contains(page(jdx))) {
              valid = false
            }
          }
    }

    valid

given day5part1Solution: Solver[5, 1] = new Solver[5, 1] with Day5Common:

  override def solve(input: (Vector[(Int, Int)], Vector[Vector[Int]])): Int =
    val rules = toMultiMap(input._1)

    input._2.foldLeft(0) { case (sum, page) =>
      assert(page.length % 2 == 1)

      if isPageValid(page, rules) then sum + page((page.length - 1) / 2)
      else sum
    }

given day5part2Solution: Solver[5, 2] = new Solver[5, 2] with Day5Common:
  override def solve(input: (Vector[(Int, Int)], Vector[Vector[Int]])): Int =
    val rules = toMultiMap(input._1)

    input._2.foldLeft(0) { case (sum, page) =>
      assert(page.length % 2 == 1)

      if (isPageValid(page, rules)) {
        sum
      } else {
        val sorted = page.sortWith { case (l, r) =>
          rules.get(l) match
            case None =>
              rules.get(r) match
                case None         => true
                case Some(afters) => !afters.contains(l)
            case Some(afters) => afters.contains(r)
        }

        sum + sorted((sorted.length - 1) / 2)
      }
    }
