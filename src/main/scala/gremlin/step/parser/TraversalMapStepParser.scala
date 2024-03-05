package gremlin.step.parser

import scala.util.chaining._

private[parser] trait TraversalMapStepParser extends BasicParser {

  private case class TraversalMapStep(steps: Seq[QStep]) extends QStep {
    def toQuery(ind: Int): Query =
      QueryHasLeavesSource(
        pre = Some(QueryRaw(".map(", ind)),
        leaves = steps
          .map(_.toQuery(ind + 1))
          .pipe(prependReceiverToFirstQuery),
        post = Some(QueryRaw(")", ind)),
        ind = ind
      )

  }

  def traversalMapStepDelegate(p: => Parser[QStep]): Parser[QStep] = {
    "TraversalMapStep(value(original_id))" ^^ { case _ =>
      TraversalMapStep(Nil)
    }
    "TraversalMapStep([" ~> repsep(p, ",") <~ "])" ^^ (steps =>
      TraversalMapStep(steps)
    )
  }
}
