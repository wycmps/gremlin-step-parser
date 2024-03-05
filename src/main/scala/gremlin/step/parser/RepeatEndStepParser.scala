package gremlin.step.parser

import scala.util.parsing.combinator.RegexParsers

private[parser] trait RepeatEndStepParser extends RegexParsers {

  private case class RepeatEndStep() extends QStep {
    def toQuery(ind: Int): Query = QueryNone()
  }

  def repeatEndStep: Parser[QStep] =
    "RepeatEndStep" ^^ (_ => RepeatEndStep())
}
