package gremlin.step.parser

import scala.util.parsing.combinator.RegexParsers

private[parser] trait OrderGlobalStepParser extends RegexParsers {

  private case class OrderGlobalStep() extends QStep {
    def toQuery(ind: Int): Query =
      QueryRaw(""".order().by("order", asc)""", ind)
  }

  def orderGlobalStep: Parser[QStep] =
    "OrderGlobalStep([[value(order), asc]])" ^^ (_ => OrderGlobalStep())
}
