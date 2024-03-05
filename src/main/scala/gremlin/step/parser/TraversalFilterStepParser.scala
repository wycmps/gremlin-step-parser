package gremlin.step.parser

private[parser] trait TraversalFilterStepParser extends BasicParser {

  private case class TraversalFilterStep(steps: Seq[QStep]) extends QStep {
    def toQuery(ind: Int): Query =
      QueryHasLeavesSource(
        pre = Some(QueryRaw(".filter(", ind)),
        leaves = prependReceiverToFirstQuery(steps.map(_.toQuery(ind + 1))),
        post = Some(QueryRaw(")", ind)),
        ind = ind
      )
  }

  def traversalFilterStepDelegate(p: => Parser[QStep]): Parser[QStep] =
    "TraversalFilterStep([" ~> repsep(p, ",") <~ "])" ^^ (_steps =>
      TraversalFilterStep(_steps)
    )

}
