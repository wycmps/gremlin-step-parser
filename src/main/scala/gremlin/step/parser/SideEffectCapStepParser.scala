package gremlin.step.parser

import scala.util.parsing.combinator.RegexParsers

private[parser] trait SideEffectCapStepParser
    extends RegexParsers
    with BasicParser {

  private case class SideEffectCapStep(elem: QElem) extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(s".cap(${elem.toQuery})", ind)
  }

  def sideEffectCapStep: Parser[QStep] =
    "SideEffectCapStep([" ~> word <~ "])" ^^ (e => SideEffectCapStep(e))
}
