package advent

import scala.annotation.targetName

case class Pos(row: Long, col: Long) derives CanEqual:
  @targetName("addPos")
  def +(other: Pos): Pos =
    Pos(row + other.row, col + other.col)

  @targetName("subPos")
  def -(other: Pos): Pos =
    Pos(row - other.row, col - other.col)

  def abs: Pos =
    Pos(Math.abs(row), Math.abs(col))

  def isInBox(width: Long, height: Long): Boolean =
    row >= 0 && row < height && col >= 0 && col < width
