package advent

case class InvalidLineError(line: String, message: String = "")
    extends Exception(
      s"Invalid line: `${line}`${
          if message.nonEmpty then s"\n${message}" else ""
        }",
    )

case class InvalidInputError(message: String)
    extends Exception(s"Invalid input: ${message}")
