package gremlin.step.parser

import scala.util.chaining._

private[parser] trait OrderGlobalStepParser extends BasicParser {

  private case class OrderGlobalStepSimple(label: QElem, sort: QElem) extends QStep {
    def toQuery(ind: Int): Query =
      QueryRaw(s""".order().by(${label.toQuery}, ${sort.rawString})""", ind)
  }

  private case class OrderGlobalStepNested(steps: Seq[QStep], sort: QElem) extends QStep {
    def toQuery(ind: Int): Query =
      QueryHasLeavesSource(
        pre = Some(QueryRaw(".order().by(", ind)),
        leaves = prependReceiverToFirstQuery(steps.map(_.toQuery(ind + 1)))
          .pipe { l => l.init :+ l.last.withStepSeparator() :+ QueryRaw(s"""${sort.rawString}""", ind + 1) },
        post = Some(QueryRaw(s")", ind)),
        ind = ind
      )
  }

  private def innerSteps(p: => Parser[QStep]): Parser[Seq[QStep]] =
    "[" ~> repsep(p, ",") <~ "]"

  private def nested(p: => Parser[QStep]): Parser[QStep] =
    "[" ~> innerSteps(p) ~ "," ~ word <~ "]" ^^ { case steps ~ _ ~ sort =>
      OrderGlobalStepNested(steps, sort)
    }

  def orderGlobalStepDelegate(p: => Parser[QStep]): Parser[QStep] = {
    "OrderGlobalStep([" ~> nested(p) <~ "])" |
      "OrderGlobalStep([[value(" ~> word ~ ")," ~ word <~ "]])" ^^ { case label ~ _ ~ sort =>
        OrderGlobalStepSimple(label, sort)
      }
  }
}
