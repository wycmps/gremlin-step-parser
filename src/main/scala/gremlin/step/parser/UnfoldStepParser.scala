package gremlin.step.parser

private[parser] trait UnfoldStepParser extends BasicParser {

  private case class UnfoldStep() extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(".unfold()", ind)
  }

  def unfoldStep: Parser[QStep] =
    "UnfoldStep" ^^ (_ => UnfoldStep())
}
