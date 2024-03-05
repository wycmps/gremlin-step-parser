package gremlin.step.parser

private[parser] trait LambdaMapStepParser extends BasicParser {

  private case class LambdaMapStep() extends QStep {
    def toQuery(ind: Int): Query = QueryNone()
  }

  def lambdaMapStep: Parser[QStep] =
    "LambdaMapStep(lambda)" ^^ (_ => LambdaMapStep())
}
