package gremlin.step.parser

private[parser] trait PropertiesStepParser extends BasicParser {

  private case class PropertiesStep(elem: QElem) extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(s".values(${elem.toQuery})", ind)
  }

  def propertiesStep: Parser[QStep] =
    "PropertiesStep([" ~> word <~ "],value)" ^^ (e => PropertiesStep(e))
}
