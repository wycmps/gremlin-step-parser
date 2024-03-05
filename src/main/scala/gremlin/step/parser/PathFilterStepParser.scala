package gremlin.step.parser

private[parser] trait PathFilterStepParser extends BasicParser {

  private case class PathFilterStep() extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(".simplePath()", ind)
  }

  def pathFilterStep: Parser[QStep] =
    "PathFilterStep(simple,null,null)" ^^ (_ => PathFilterStep())
}
