package gremlin.step.parser

private[parser] trait RepeatStepParser extends BasicParser {

  private case class RepeatUntilStep(
      emit: Boolean,
      repeatSteps: Seq[QStep],
      untilSteps: Seq[QStep]
  ) extends QStep {
    def toQuery(ind: Int): Query =
      QueryHasTwoLeavesSource(
        pre = Some(
          QueryRaws(
            (if (emit) Seq(".emit()") else Nil) :+ ".repeat(",
            ind
          )
        ),
        leaves1 =
          prependReceiverToFirstQuery(repeatSteps.map(_.toQuery(ind + 1))),
        mid =
          if (untilSteps.isEmpty) None
          else
            Some(
              QueryRaws(
                Seq(")", ".until("),
                ind
              )
            ),
        leaves2 =
          prependReceiverToFirstQuery(untilSteps.map(_.toQuery(ind + 1))),
        post = Some(QueryRaw(")", ind)),
        ind = ind
      )
  }

  private case class UntilRepeatStep(
      untilSteps: Seq[QStep],
      repeatSteps: Seq[QStep]
  ) extends QStep {
    def toQuery(ind: Int): Query =
      QueryHasTwoLeavesSource(
        pre = Some(
          QueryRaws(
            Seq(".emit()", ".until("),
            ind
          )
        ),
        leaves1 =
          prependReceiverToFirstQuery(repeatSteps.map(_.toQuery(ind + 1))),
        mid = Some(
          QueryRaws(
            Seq(")", ".repeat("),
            ind
          )
        ),
        leaves2 =
          prependReceiverToFirstQuery(untilSteps.map(_.toQuery(ind + 1))),
        post = Some(QueryRaw(")", ind)),
        ind = ind
      )
  }

  private def emitT: Parser[Boolean] = "emit(" ~> boolT <~ ")"
  private def emitF: Parser[Boolean] = "emit(" ~> boolF <~ ")"
  private def repeat(p: => Parser[QStep]): Parser[Seq[QStep]] =
    "[" ~> repsep(p, ",") <~ "]"
  private def until(p: => Parser[QStep]): Parser[Seq[QStep]] =
    "until([" ~> repsep(p, ",") <~ "])" |
      "until(false)" ^^ (_ => Nil)

  def repeatStepDelegate(p: => Parser[QStep]): Parser[QStep] =
    "RepeatStep(" ~> emitT ~ "," ~ repeat(p) ~ "," ~ until(p) <~ ")" ^^ {
      case emitT ~ _ ~ repeatSteps ~ _ ~ untilSteps =>
        RepeatUntilStep(emitT, repeatSteps, untilSteps)
    } |
      "RepeatStep(" ~> until(p) ~ "," ~ emitT ~ "," ~ repeat(p) <~ ")" ^^ {
        case untilSteps ~ _ ~ _ ~ _ ~ repeatSteps =>
          UntilRepeatStep(untilSteps, repeatSteps)
      } |
      "RepeatStep(" ~> repeat(p) ~ "," ~ until(p) ~ "," ~ emitF <~ ")" ^^ {
        case repeatSteps ~ _ ~ untilSteps ~ _ ~ emitF =>
          RepeatUntilStep(emitF, repeatSteps, untilSteps)
      }
}
