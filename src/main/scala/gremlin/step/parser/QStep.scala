package gremlin.step.parser

private[parser] trait QStep {
  def toQuery(ind: Int): Query
  def prependReceiverToFirstQuery(qs: Seq[Query]): Seq[Query] =
    qs match {
      case head :: tail => head.withReceiver() +: tail
      case _            => qs
    }

  def mapWithStepSeparator(qs: Seq[Query]): Seq[Query] =
    qs match {
      case init :+ last => init.map(_.withStepSeparator()) :+ last
      case l            => l
    }
}
