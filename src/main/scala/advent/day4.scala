package advent

trait Day4Common:
  type Input = Vector[Vector[Char]]

  val parser: Parser[Input] = PBasic(RChars())

given day4part1Solution: Solver[4, 1] = new Solver[4, 1] with Day4Common:

  val directions = List[(Long, Long)](
    (-1, 0),
    (1, 0),
    (0, -1),
    (0, 1),
    (-1, -1),
    (1, -1),
    (-1, 1),
    (1, 1),
  )

  override def solve(matrix: Vector[Vector[Char]]): Long =
    val height = matrix.length
    val width = matrix.head.length

    println(matrix.map(_.mkString(",")).mkString("\n"))

    def isPositionValid(position: (Long, Long)): Boolean =
      val (row, col) = position

      row >= 0 && row < height && col >= 0 && col < width

    def getXmasses(position: (Long, Long)): List[(Long, Long)] =
      directions.filter { mod =>
        val mPos = (position._1 + mod._1, position._2 + mod._2)
        val aPos = (mPos._1 + mod._1, mPos._2 + mod._2)
        val sPos = (aPos._1 + mod._1, aPos._2 + mod._2)

        val mValid =
          isPositionValid(mPos) && matrix(mPos._1.toInt)(mPos._2.toInt) == 'M'
        val aValid =
          isPositionValid(aPos) && matrix(aPos._1.toInt)(aPos._2.toInt) == 'A'
        val sValid =
          isPositionValid(sPos) && matrix(sPos._1.toInt)(sPos._2.toInt) == 'S'

        mValid && aValid && sValid
      }

    val xIndices = matrix.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.zipWithIndex.flatMap { case (col, colIndex) =>
        if col == 'X' then List((rowIndex.toLong, colIndex.toLong))
        else List.empty
      }
    }

    xIndices.foldLeft(0L) { case (count, position) =>
      count + getXmasses(position).length
    }

given day4Part2Solution: Solver[4, 2] = new Solver[4, 2] with Day4Common:

  val positions = List[((Long, Long), (Long, Long))](
    // Top Left to Bottom Right
    ((-1, -1), (1, 1)),
    // Top Right to Bottom Left
    ((-1, 1), (1, -1)),
  )

  override def solve(matrix: Vector[Vector[Char]]): Long =
    val height = matrix.length
    val width = matrix.head.length

    def isPositionValid(position: (Long, Long)): Boolean =
      val (row, col) = position

      row >= 0 && row < height && col >= 0 && col < width

    def getDiag(
        position: (Long, Long),
        transform: ((Long, Long), (Long, Long)),
    ): Option[(Char, Char)] =
      val startPos =
        (position._1 + transform._1._1, position._2 + transform._1._2)
      val endPos =
        (position._1 + transform._2._1, position._2 + transform._2._2)

      if isPositionValid(startPos) && isPositionValid(endPos) then
        Some(
          (
            matrix(startPos._1.toInt)(startPos._2.toInt),
            matrix(endPos._1.toInt)(endPos._2.toInt),
          ),
        )
      else None

    val aIndices = matrix.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.zipWithIndex.flatMap { case (col, colIndex) =>
        if col == 'A' then List((rowIndex.toLong, colIndex.toLong))
        else List.empty
      }
    }

    aIndices.foldLeft(0L) { case (count, position) =>
      var valid = 0L

      for (pos <- positions) {
        getDiag(position, pos) match
          case Some((c1, c2)) if valid < 2 =>
            if (c1 == 'M' && c2 == 'S') || (c1 == 'S' && c2 == 'M') then
              valid += 1
            else ()
          case _ => ()
      }

      if (valid == 2) {
        count + 1
      } else { count }
    }
