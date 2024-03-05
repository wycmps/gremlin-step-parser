package gremlin.step.parser

private[parser] trait SubgraphStepParser extends BasicParser {

  private case class SubgraphStep(elem: QElem) extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(s".subgraph(${elem.toQuery})", ind)
  }

  def subgraphStep: Parser[QStep] =
    "SubgraphStep(" ~> word <~ ")" ^^ (e => SubgraphStep(e))
}
