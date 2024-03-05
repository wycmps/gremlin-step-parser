package gremlin.step.parser

private[parser] trait FoldStepParser extends BasicParser {

  private case class FoldStep() extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(".fold()", ind)
  }

  def foldStep: Parser[QStep] =
    "FoldStep" ^^ (_ => FoldStep())
}
