package advent

def parseInput(
    input: Vector[String],
): (Map[Int, Set[Int]], Vector[Array[Int]]) =
  def toMultiMap(rules: Vector[(Int, Int)]): Map[Int, Set[Int]] =
    rules.foldLeft(Map.empty[Int, Set[Int]]) { case (map, (before, after)) =>
      map.updatedWith(before) {
        case None      => Some(Set(after))
        case Some(set) => Some(set + after)
      }
    }

  input.splitAt(input.indexWhere(_.isBlank)) match
    case (rawRules, rawPages) =>
      (
        toMultiMap(rawRules.map { rawRule =>
          rawRule.split("\\|") match
            case Array(before, after) => before.toInt -> after.toInt
            case _                    => throw InvalidLineError(rawRule)
        }),
        rawPages.drop(1).map(_.split(",").map(_.toInt)),
      )

def isPageValid(page: Array[Int], rules: Map[Int, Set[Int]]): Boolean =
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

given day5part1Solution: Solver[5, 1] = new Solver[5, 1]:

  override def solve(input: Vector[String]): Int =
    val (rules, pages) = parseInput(input)

    pages.foldLeft(0) { case (sum, page) =>
      assert(page.length % 2 == 1)

      if isPageValid(page, rules) then sum + page((page.length - 1) / 2)
      else sum
    }

given day5part2Solution: Solver[5, 2] = new Solver[5, 2]:
  override def solve(input: Vector[String]): Int =
    val (rules, pages) = parseInput(input)

    pages.foldLeft(0) { case (sum, page) =>
      assert(page.length % 2 == 1)

      if (isPageValid(page, rules)) {
        sum
      } else {
        val sorted = page.sortWith { case (l, r) =>
          rules.get(l) match
            case None         => true
            case Some(afters) => afters.contains(r)
        }

        sum + sorted((sorted.length - 1) / 2)
      }
    }
