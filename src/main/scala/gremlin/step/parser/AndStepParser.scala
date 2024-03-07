package gremlin.step.parser

import scala.util.chaining._

private[parser] trait AndStepParser extends BasicParser {

  private case class AndStep(stepsSeq: Seq[Seq[QStep]]) extends QStep {
    def toQuery(ind: Int): Query =
      QueryHasLeavesSource(
        pre = Some(QueryRaw(".and(", ind)),
        leaves = stepsSeq
          .map { ss =>
            QueryHasLeavesSource(
              pre = None,
              leaves = prependReceiverToFirstQuery(ss.map(_.toQuery(ind + 1))),
              post = None,
              ind = ind
            )
          }
          .pipe(mapWithStepSeparator),
        post = Some(QueryRaw(")", ind)),
        ind = ind
      )
  }

  private def innerSteps(p: => Parser[QStep]): Parser[Seq[QStep]] =
    "[" ~> repsep(p, ",") <~ "]"

  private def outerSteps(p: => Parser[QStep]): Parser[Seq[Seq[QStep]]] =
    "[" ~> repsep(innerSteps(p), ",") <~ "]"

  def andStepDelegate(p: => Parser[QStep]): Parser[QStep] =
    "AndStep(" ~> outerSteps(p) <~ ")" ^^ (stepsSeq => AndStep(stepsSeq))
}
