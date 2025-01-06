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
    case Con

  enum Exp derives CanEqual:
    case BinOp(op: Op)
    case Num(value: Long)

  object Exp:
    def format: Exp => String =
      case BinOp(Op.Add) => "+"
      case BinOp(Op.Mul) => "*"
      case BinOp(Op.Con) => "||"
      case Num(value)    => value.toString

    def eval(exps: Vector[Exp]): Long =
      exps
        .foldLeft((Option.empty[Op], 0L)) {
          case ((None, value), BinOp(op))          => (Some(op), value)
          case ((None, _), Num(value))             => (None, value)
          case ((Some(Op.Add), soFar), Num(value)) => (None, value + soFar)
          case ((Some(Op.Mul), soFar), Num(value)) => (None, value * soFar)
          case ((Some(Op.Con), soFar), Num(value)) =>
            (None, (soFar.toString + value.toString).toLong)
          case ((Some(_), _), BinOp(_)) =>
            assert(false, "Got BinOp, Num expected")
        }
        ._2

  def allPerms[T](values: Vector[T], size: Long): Vector[Vector[T]] =
    List
      .fill(size.toInt)(values)
      .flatten
      .combinations(size.toInt)
      .flatMap(_.permutations)
      .map(_.toVector)
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

  def createExpressions(
      parts: Vector[Long],
      expressionSet: Vector[Op],
  ): Vector[Vector[Exp]] =
    val size = parts.length
    val limit = Math.pow(2, size - 1).toInt
    assert(size > 0)

    allPerms(expressionSet, parts.length - 1).map { ops =>
      intersperseOps(parts, ops)
    }

  def solveFor(input: Input, expressionSet: Vector[Op]): Long =
    input.foldLeft(0L) { case (sum, (target, parts)) =>
      if (
        createExpressions(parts, expressionSet)
          .exists(exp => Exp.eval(exp) == target)
      ) {
        sum + target
      } else {
        sum
      }
    }

given day7part1Solution: Solver[7, 1] = new Solver[7, 1] with Day7Common:

  override def solve(input: Vector[(Long, Vector[Long])]): Long =
    solveFor(input, Vector(Op.Add, Op.Mul))

given day7part2Solution: Solver[7, 2] = new Solver[7, 2] with Day7Common:
  override def solve(input: Vector[(Long, Vector[Long])]): Long =
    solveFor(input, Vector(Op.Add, Op.Mul, Op.Con))
