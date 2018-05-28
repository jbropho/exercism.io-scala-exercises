object Bob extends App {

  def response(statement: String): String = {
    val shouting = isShouting(statement)
    val isEmpty = isEmptyStatement(statement)
    val question = isQuestion(statement)
    val shoutingQuestion = shouting && question

    true match {
      case `shoutingQuestion` => "Calm down, I know what I'm doing!"
      case `isEmpty` => "Fine. Be that way!"
      case `shouting` => "Whoa, chill out!"
      case `question` => "Sure."
      case _ => "Whatever."
    }
  }

  def isShouting(statement: String): Boolean = {
      if (!containsLetters(statement)) return false
      statement.toUpperCase == statement
  }

  def containsLetters(statement: String): Boolean = {
      statement.exists(_.isLetter)
  }

  def isEmptyStatement(statement: String): Boolean = {
      statement.trim.length == 0
  }

  def isQuestion(statement: String): Boolean = {
      (statement.trim takeRight 1) == "?"
  }
}
