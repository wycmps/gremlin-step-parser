package gremlin.step.parser

private[parser] trait GroupStepParser extends BasicParser {

  private case class GroupStep(value: QElem, steps: Seq[QStep]) extends QStep {
    def toQuery(ind: Int): Query =
      QueryHasLeavesSource(
        pre = Some(
          QueryRaws(
            Seq(".group()", s".by(${value.toQuery})", ".by("),
            ind
          )
        ),
        leaves = prependReceiverToFirstQuery(steps.map(_.toQuery(ind + 1))),
        post = Some(QueryRaw(")", ind)),
        ind = ind
      )
  }

  private case class GroupValueStepsStep(
      valueSteps: Seq[QStep],
      steps: Seq[QStep]
  ) extends QStep {
    def toQuery(ind: Int): Query =
      QueryHasTwoLeavesSource(
        pre = Some(
          QueryRaws(
            Seq(".group()", s".by("),
            ind
          )
        ),
        leaves1 = valueSteps.map(_.toQuery(ind + 1)),
        mid = Some(
          QueryRaws(
            Seq(")", ".by("),
            ind
          )
        ),
        leaves2 = steps.map(_.toQuery(ind + 1)),
        post = Some(QueryRaw(")", ind)),
        ind = ind
      )
  }
  private def value: Parser[QElem] = "value(" ~> word <~ ")"
  private def repeat(p: => Parser[QStep]): Parser[Seq[QStep]] =
    "[" ~> repsep(p, ",") <~ "]"

  def groupStepDelegate(p: => Parser[QStep]): Parser[QStep] =
    "GroupStep(" ~> value ~ "," ~ repeat(p) <~ ")" ^^ {
      case value ~ _ ~ _steps => GroupStep(value, _steps)
    } | "GroupStep(" ~> repeat(p) ~ "," ~ repeat(p) <~ ")" ^^ {
      case valueSteps ~ _ ~ _steps => GroupValueStepsStep(valueSteps, _steps)
    }
}
