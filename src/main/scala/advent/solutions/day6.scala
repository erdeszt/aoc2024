package advent.solutions

import scala.annotation.tailrec

import advent.*

trait Day6Common:
  type Input = Vector[Vector[Char]]

  val parser: Parser[Input] = PBasic(RChars())

  case class Pos(row: Long, col: Long)

  enum Dir derives CanEqual:
    case Up
    case Down
    case Left
    case Right

    def turnRight: Dir =
      this match
        case Up    => Right
        case Down  => Left
        case Left  => Up
        case Right => Down

  def getNextPos(pos: Pos, dir: Dir): Pos =
    dir match
      case Dir.Up    => pos.copy(row = pos.row - 1)
      case Dir.Down  => pos.copy(row = pos.row + 1)
      case Dir.Left  => pos.copy(col = pos.col - 1)
      case Dir.Right => pos.copy(col = pos.col + 1)

  case class Run[T](value: T, runLength: Long) derives CanEqual

  enum Action derives CanEqual:
    case Move(dir: Dir)
    case Turn(dir: Dir)

  type RouteF[F[_]] = Vector[F[Action]]
  type Id[T] = T
  type Route = RouteF[Id]
  type RouteRLE = RouteF[Run]

  def rle(route: Route): RouteRLE =
    route
      .foldLeft((Vector.empty[Run[Action]], Option.empty[Run[Action]])) {
        case ((rled, None), action) =>
          (rled, Some(Run(action, 1)))
        case ((rled, Some(Run(currentAction, runLength))), action) =>
          if currentAction == action then
            (
              rled,
              Some(Run(currentAction, runLength + 1)),
            )
          else (rled :+ Run(currentAction, runLength), Some(Run(action, 1)))
      }
      ._1

given day6part1Solution: Solver[6, 1] = new Solver[6, 1] with Day6Common:
  override def solve(input: Vector[Vector[Char]]): Long =
    assert(input.nonEmpty)

    val startPos = input.zipWithIndex
      .foldLeft(Option.empty[Pos]) {
        case (None, (row, rowIndex)) =>
          row.indexWhere(_ == '^') match
            case -1       => None
            case colIndex => Some(Pos(rowIndex, colIndex))
        case (result @ Some(_), _) => result
      }
      .get
    val height = input.length
    val width = input.head.length
    var route = Vector.empty[Action]

    @tailrec
    def go(currentPos: Pos, dir: Dir, visited: Set[Pos]): Long = {
      val nextPos = getNextPos(currentPos, dir)

      if (
        nextPos.row < 0 || nextPos.row >= height || nextPos.col < 0 || nextPos.col >= width
      ) {
        visited.size
      } else if (input(nextPos.row.toInt)(nextPos.col.toInt) == '#') {
        route = route :+ Action.Turn(dir.turnRight)
        go(currentPos, dir.turnRight, visited)
      } else {
        route = route :+ Action.Move(dir)
        go(nextPos, dir, visited + nextPos)
      }
    }

    val result = go(startPos, Dir.Up, Set(startPos))

    val routeRle = rle(route)

    result

given day6part2Solution: Solver[6, 2] = new Solver[6, 2] with Day6Common:
  override def solve(input: Vector[Vector[Char]]): Long =
    ???
