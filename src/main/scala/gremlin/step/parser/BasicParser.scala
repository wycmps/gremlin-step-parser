package gremlin.step.parser

import scala.util.parsing.combinator.RegexParsers

private[parser] trait QElem {
  val rawString: String
  def toQuery: String
}

private[parser] case class QElemString(rawString: String) extends QElem {
  def toQuery: String = s""""$rawString""""
}

private[parser] case class QElemNumber(rawString: String) extends QElem {
  def toQuery: String = s"""$rawString"""
}

private[parser] trait BasicParser extends RegexParsers {

  def word: Parser[QElem] = """\w+""".r ^^ (s => QElemString(s))
  def number: Parser[QElem] = """\d+""".r ^^ (s => QElemNumber(s))
  def uuid: Parser[String] = """[0-9a-fA-F-]+""".r
  def boolT: Parser[Boolean] = "true" ^^ (_ => true)
  def boolF: Parser[Boolean] = "false" ^^ (_ => false)
}
