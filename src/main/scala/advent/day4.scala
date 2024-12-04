package advent

enum Dir derives CanEqual:
  case Up
  case Down
  case Left
  case Right
  case LeftUp
  case LeftDown
  case RightUp
  case RightDown

object Dir:
  val all: List[Dir] =
    List(Up, Down, Left, Right, LeftUp, LeftDown, RightUp, RightDown)

  def toPositionModifier: Dir => (Int, Int) = {
    case Up        => (-1, 0)
    case Down      => (1, 0)
    case Left      => (0, -1)
    case Right     => (0, 1)
    case LeftUp    => (-1, -1)
    case LeftDown  => (1, -1)
    case RightUp   => (-1, 1)
    case RightDown => (1, 1)
  }

given day4part1Solution: Solver[4, 1] = new Solver[4, 1]:
  override def solve(input: Vector[String]): Int =
    val matrix = input.map(_.toCharArray).toArray
    val height = matrix.length
    val width = matrix.head.length

    def isPositionValid(position: (Int, Int)): Boolean =
      val (row, col) = position

      row >= 0 && row < height && col >= 0 && col < width

    def getXmasses(position: (Int, Int)): List[Dir] =
      Dir.all.filter { dir =>
        val mod = Dir.toPositionModifier(dir)
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
