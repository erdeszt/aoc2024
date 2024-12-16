package advent

sealed trait Parser[T]
case class PBasic[T](rowPattern: RowPattern[T]) extends Parser[Vector[T]]
case class PThen[T, U](first: Parser[T], second: Parser[U])
    extends Parser[(T, U)]

object Parser:
  def parse[T](parser: Parser[T])(rows: Vector[String]): T =
    parser match
      case PBasic(rowPattern) => rows.map(RowPattern.parse(rowPattern))
      case PThen(firstParser, secondParser) =>
        rows.splitAt(rows.indexWhere(_.isBlank)) match
          case (first, second) =>
            (
              Parser.parse(firstParser)(first),
              Parser.parse(secondParser)(second.drop(1)),
            )

sealed trait RowPattern[T]
case class RTuple[T, U](
    separator: Separator,
    leftParser: ValueParser[T],
    rightParser: ValueParser[U],
) extends RowPattern[(T, U)]
case class RArray[T](separator: Separator, valueParser: ValueParser[T])
    extends RowPattern[Vector[T]]
case class RString() extends RowPattern[String]
case class RChars() extends RowPattern[Vector[Char]]

object RowPattern:
  def parse[T](parser: RowPattern[T])(row: String): T =
    parser match
      case RTuple(separator, leftParser, rightParser) =>
        row.split(separator.toSplitPattern) match
          case scala.Array(left, right) =>
            (
              ValueParser.parse(leftParser)(left),
              ValueParser.parse(rightParser)(right),
            )
          case _ => throw InvalidLineError(row)
      case RArray(separator, valueParser) =>
        row
          .split(separator.toSplitPattern)
          .toVector
          .map(ValueParser.parse(valueParser))
      case RString() =>
        row
      case RChars() =>
        row.toCharArray.toVector

sealed trait ValueParser[T]
case class VInt() extends ValueParser[Int]
case class VChar() extends ValueParser[Char]

object ValueParser:
  def parse[T](parser: ValueParser[T])(raw: String): T =
    parser match
      case VInt() => raw.toInt
      case VChar() =>
        assert(raw.length == 1)
        raw(0)

enum Separator derives CanEqual:
  case Comma
  case Whitespace
  case Pipe

  def toSplitPattern: String =
    this match
      case Comma      => ","
      case Whitespace => "\\s+"
      case Pipe       => "\\|"
