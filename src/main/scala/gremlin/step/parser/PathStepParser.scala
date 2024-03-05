package gremlin.step.parser

private[parser] trait PathStepParser extends BasicParser {

  private case class PathStep() extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(".path()", ind)
  }

  def pathStep: Parser[QStep] =
    "PathStep" ^^ (_ => PathStep())
}
