package gremlin.step.parser

private[parser] trait InjectStepParser extends BasicParser {

  private case class InjectStep(elem: QElem) extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(s"g.V([${elem.toQuery}])", ind)
  }

  private case class EmptyInjectStep() extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(s"g.V()", ind)
  }

  def injectStep: Parser[QStep] =
    "InjectStep([])" ^^ (_ => EmptyInjectStep()) |
      "InjectStep([v[" ~> number <~ "]])" ^^ (e => InjectStep(e))
}
