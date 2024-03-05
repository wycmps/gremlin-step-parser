package gremlin.step.parser

private[parser] trait SelectOneStepParser extends BasicParser {

  private case class SelectOneStep() extends QStep {
    def toQuery(ind: Int): Query =
      QueryRaw(s"SelectOneStep TODO", ind)
  }
  def selectOneStep: Parser[QStep] =
    "SelectOneStep(last,atom,null)" ^^ { case _ =>
      SelectOneStep()
    }
}
