package gremlin.step.parser

import scala.util.parsing.combinator.RegexParsers

private[parser] trait EdgeVertexStepParser extends RegexParsers {

  sealed private trait OP {
    def toQuery: String
  }
  private case class OutV() extends OP {
    override def toQuery: String = ".outV()"
  }

  private case class InV() extends OP {
    override def toQuery: String = ".inV()"
  }
  private case class EdgeVertexStep(op: OP) extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(op.toQuery, ind)
  }

  def edgeVertexStep: Parser[QStep] =
    "EdgeVertexStep(OUT)" ^^ (_ => EdgeVertexStep(OutV())) |
      "EdgeVertexStep(IN)" ^^ (_ => EdgeVertexStep(InV()))
}
