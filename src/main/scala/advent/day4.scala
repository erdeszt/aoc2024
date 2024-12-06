package advent

given day4part1Solution: Solver[4, 1] = new Solver[4, 1]:

  val directions = List(
    (-1, 0),
    (1, 0),
    (0, -1),
    (0, 1),
    (-1, -1),
    (1, -1),
    (-1, 1),
    (1, 1),
  )

  override def solve(input: Vector[String]): Int =
    val matrix = input.map(_.toCharArray).toArray
    val height = matrix.length
    val width = matrix.head.length

    def isPositionValid(position: (Int, Int)): Boolean =
      val (row, col) = position

      row >= 0 && row < height && col >= 0 && col < width

    def getXmasses(position: (Int, Int)): List[(Int, Int)] =
      directions.filter { mod =>
        val mPos = (position._1 + mod._1, position._2 + mod._2)
        val aPos = (mPos._1 + mod._1, mPos._2 + mod._2)
        val sPos = (aPos._1 + mod._1, aPos._2 + mod._2)

        val mValid = isPositionValid(mPos) && matrix(mPos._1)(mPos._2) == 'M'
        val aValid = isPositionValid(aPos) && matrix(aPos._1)(aPos._2) == 'A'
        val sValid = isPositionValid(sPos) && matrix(sPos._1)(sPos._2) == 'S'

        mValid && aValid && sValid
      }

    val xIndices = matrix.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.zipWithIndex.flatMap { case (col, colIndex) =>
        if col == 'X' then List((rowIndex, colIndex)) else List.empty
      }
    }

    xIndices.foldLeft(0) { case (count, position) =>
      count + getXmasses(position).length
    }

given day4Part2Solution: Solver[4, 2] = new Solver[4, 2]:

  val positions = List(
    // Top Left to Bottom Right
    ((-1, -1), (1, 1)),
    // Top Right to Bottom Left
    ((-1, 1), (1, -1)),
  )

  override def solve(input: Vector[String]): Int =
    val matrix = input.map(_.toCharArray).toArray
    val height = matrix.length
    val width = matrix.head.length

    def isPositionValid(position: (Int, Int)): Boolean =
      val (row, col) = position

      row >= 0 && row < height && col >= 0 && col < width

    def getDiag(
        position: (Int, Int),
        transform: ((Int, Int), (Int, Int)),
    ): Option[(Char, Char)] =
      val startPos =
        (position._1 + transform._1._1, position._2 + transform._1._2)
      val endPos =
        (position._1 + transform._2._1, position._2 + transform._2._2)

      if isPositionValid(startPos) && isPositionValid(endPos) then
        Some((matrix(startPos._1)(startPos._2), matrix(endPos._1)(endPos._2)))
      else None

    val aIndices = matrix.zipWithIndex.flatMap { case (row, rowIndex) =>
      row.zipWithIndex.flatMap { case (col, colIndex) =>
        if col == 'A' then List((rowIndex, colIndex)) else List.empty
      }
    }

    aIndices.foldLeft(0) { case (count, position) =>
      var valid = 0

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
