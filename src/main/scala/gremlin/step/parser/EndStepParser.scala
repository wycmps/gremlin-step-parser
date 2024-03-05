package gremlin.step.parser

private[parser] trait EndStepParser extends BasicParser {

  private case class EndStep() extends QStep {
    def toQuery(ind: Int): Query = QueryNone()
  }

  def endStep: Parser[QStep] =
    "EndStep" ^^ (_ => EndStep())
}
