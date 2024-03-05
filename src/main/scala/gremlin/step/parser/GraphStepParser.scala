package gremlin.step.parser

private[parser] trait GraphStepParser extends BasicParser {

  private case class GraphStep(ids: Seq[QElem], aliasOpt: Option[QElem])
      extends QStep {
    def toQuery(ind: Int): Query = {
      val sIds =
        if (ids.isEmpty) "" else ids.map(_.toQuery).mkString("[", ",", "]")
      QueryRaw(
        s"g.V($sIds)${aliasOpt.fold("")(a => s".as(${a.toQuery})")}",
        ind
      )
    }
  }

  def ids: Parser[Seq[QElem]] =
    "[]" ^^ (_ => Nil) |
      "[" ~> repsep("v[" ~> number <~ "]", ",") <~ "]"

  def graphStep: Parser[QStep] =
    "GraphStep(vertex," ~ ids ~ ")" ~ opt("@[" ~> word <~ "]") ^^ {
      case _ ~ ids ~ _ ~ alias => GraphStep(ids, alias)
    }
}
