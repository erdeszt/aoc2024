package advent

import scala.util.Using

trait Solver[Day <: Int: ValueOf, Part <: Int: ValueOf]:

  type Input

  val parser: Parser[Input]

  /** Solves the `Part` of the solution for the `Day`
    *
    * @param input
    *   The lines of the input file
    * @return
    *   The solution
    */
  def solve(input: Input): Long

object Solver:

  def solve[Day <: Int: ValueOf, Part <: Int: ValueOf](using
      solver: Solver[Day, Part],
  ): Unit =
    val rawInput = Using(
      scala.io.Source.fromResource(
        s"inputs/day${valueOf[Day]}.txt",
      ),
    )(_.getLines().toVector).get
    val input = Parser.parse(solver.parser)(rawInput)

    val solution = solver.solve(input)

    println(
      s"Solution to Day ${valueOf[Day]}, part ${valueOf[Part]}: ${solution}",
    )
