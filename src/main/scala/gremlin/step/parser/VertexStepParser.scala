package gremlin.step.parser

private[parser] trait VertexStepParser extends BasicParser {

  sealed private trait OP {
    def toQuery: String
  }

  private def qfrom(elems: Seq[QElem]): String =
    if (elems.isEmpty) "" else elems.map(_.toQuery).mkString(", ")

  private case class OutVertex(elems: Seq[QElem]) extends OP {
    def toQuery: String = s".out(${qfrom(elems)})"
  }

  private case class InEdge(elems: Seq[QElem]) extends OP {
    def toQuery: String = s".inE(${qfrom(elems)})"
  }

  private case class OutEdge(elems: Seq[QElem]) extends OP {
    def toQuery: String = s".outE(${qfrom(elems)})"
  }

  private case class InVertex(elems: Seq[QElem]) extends OP {
    def toQuery: String = s".in(${qfrom(elems)})"
  }

  private case class VertexStep(op: OP) extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(op.toQuery, ind)
  }

  private def elems: Parser[Seq[QElem]] = "[" ~> repsep(word, ",") <~ "]"

  def vertexStep: Parser[QStep] =
    "VertexStep(OUT,vertex)" ^^ (_ => VertexStep(OutVertex(Nil))) |
      "VertexStep(IN," ~> elems <~ ",edge)" ^^ (elems =>
        VertexStep(InEdge(elems))
      ) |
      "VertexStep(OUT," ~> elems <~ ",edge)" ^^ (elems =>
        VertexStep(OutEdge(elems))
      ) |
      "VertexStep(IN," ~> elems <~ ",vertex)" ^^ (elems =>
        VertexStep(InVertex(elems))
      ) |
      "VertexStep(OUT," ~> elems <~ ",vertex)" ^^ (elems =>
        VertexStep(OutVertex(elems))
      )
}
