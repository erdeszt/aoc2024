package advent

import scala.annotation.tailrec

trait Day7Common:
  type Input = Vector[(Long, Vector[Long])]

  val parser: Parser[Input] = PBasic(
    RTuple(Separator.Colon, VNum(), VArray(Separator.Whitespace, VNum())),
  )

  enum Op derives CanEqual:
    case Add
    case Mul

  enum Exp derives CanEqual:
    case BinOp(op: Op)
    case Num(value: Long)

  object Exp:
    def format: Exp => String =
      case BinOp(Op.Add) => "+"
      case BinOp(Op.Mul) => "*"
      case Num(value)    => value.toString

    def eval(exps: Vector[Exp]): Long =
      exps
        .foldLeft((Option.empty[Op], 0L)) {
          case ((None, value), BinOp(op))          => (Some(op), value)
          case ((None, _), Num(value))             => (None, value)
          case ((Some(Op.Add), soFar), Num(value)) => (None, value + soFar)
          case ((Some(Op.Mul), soFar), Num(value)) => (None, value * soFar)
          case ((Some(_), _), BinOp(_)) =>
            assert(false, "Got BinOp, Num expected")
        }
        ._2

  def patternToOps(pattern: Long, size: Long): Vector[Op] =
    0L.until(size)
      .map { idx =>
        if (pattern & (1 << idx.toInt)) > 0 then Op.Add else Op.Mul
      }
      .toVector

  def intersperseOps(parts: Vector[Long], ops: Vector[Op]): Vector[Exp] =
    @tailrec
    def go(
        remainingParts: Vector[Long],
        remainingOps: Vector[Op],
        exps: Vector[Exp],
    ): Vector[Exp] =
      (remainingParts, remainingOps) match
        case (currentPart +: restOfTheParts, currentOp +: restOfTheOps) =>
          go(
            restOfTheParts,
            restOfTheOps,
            (exps :+ Exp.Num(currentPart)) :+ Exp.BinOp(currentOp),
          )
        case (currentPart +: restOfTheParts, IndexedSeq()) =>
          exps :+ Exp.Num(currentPart)
        case (IndexedSeq(), IndexedSeq()) =>
          exps
        case other => assert(false, s"Impossible value: ${other}")

    assert(parts.nonEmpty)
    assert(parts.length == ops.length + 1)

    go(parts, ops, Vector.empty)

  def createExpressions(parts: Vector[Long]): Vector[Vector[Exp]] =
    val size = parts.length
    val limit = Math.pow(2, size - 1).toInt
    assert(size > 0)

    0.until(limit)
      .map { pattern =>
        intersperseOps(parts, patternToOps(pattern, size - 1))
      }
      .toVector

given day7part1Solution: Solver[7, 1] = new Solver[7, 1] with Day7Common:

  override def solve(input: Vector[(Long, Vector[Long])]): Long =
    input.foldLeft(0L) { case (sum, (target, parts)) =>
      if (createExpressions(parts).exists(exp => Exp.eval(exp) == target)) {
        sum + target
      } else {
        sum
      }
    }

given day7part2Solution: Solver[7, 2] = new Solver[7, 2] with Day7Common:
  override def solve(input: Vector[(Long, Vector[Long])]): Long =
    ???
