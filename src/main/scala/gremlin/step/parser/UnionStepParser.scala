package gremlin.step.parser

private[parser] trait UnionStepParser extends BasicParser {

  private case class UnionStep(ora: Seq[QStep], orb: Seq[QStep]) extends QStep {
    def toQuery(ind: Int): Query =
      QueryHasLeavesSource(
        pre = Some(QueryRaw(".or(", ind)),
        leaves =
          (ora ++ orb).map(_.toQuery(ind + 1)).map(_.withReceiver()) match {
            case init :+ last => init.map(_.withStepSeparator()) :+ last
            case l            => l
          },
        post = Some(QueryRaw(")", ind)),
        ind = ind
      )
  }

  private def repeat(p: => Parser[QStep]): Parser[Seq[QStep]] =
    "[" ~> repsep(p, ",") <~ "]"

  def unionStepDelegate(p: => Parser[QStep]): Parser[QStep] =
    "UnionStep([" ~> repeat(p) ~ "," ~ repeat(p) <~ "])" ^^ {
      case ora ~ _ ~ orb => UnionStep(ora, orb)
    }
}
