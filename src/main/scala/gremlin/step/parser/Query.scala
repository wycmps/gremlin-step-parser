package gremlin.step.parser

private[parser] trait Query {
  private val CR: String = s"""\\\n"""
  val ind: Int
  val hasReceiver: Boolean
  val hasStepSeparator: Boolean
  def toLines: Seq[String]
  val indent: String = s"${" " * 2 * ind}"
  def withReceiver(): Query
  def withStepSeparator(): Query
  def line(s: String): String = s"$indent$s $CR"
}

private[parser] case class QueryRaw(
    raw: String,
    ind: Int,
    hasReceiver: Boolean = false,
    hasStepSeparator: Boolean = false
) extends Query {
  def toLines: Seq[String] = Seq(
    line(s"${if (hasReceiver) s"__" else ""}$raw${if (hasStepSeparator) s"," else ""}")
  )
  def withReceiver(): Query = copy(hasReceiver = true)
  def withStepSeparator(): Query = copy(hasStepSeparator = true)
}

private[parser] case class QueryRaws(
    raws: Seq[String],
    ind: Int,
    hasReceiver: Boolean = false,
    hasStepSeparator: Boolean = false
) extends Query {
  private def query(s: String, br: Boolean, bs: Boolean): String =
    line(s"${if (br) s"__" else ""}$s${if (bs) s"," else ""}")
  def toLines: Seq[String] =
    raws match {
      case head :: tail if hasReceiver =>
        query(head, true, false) +: tail.map(s => query(s, false, false))
      case _ => raws.map(s => query(s, false, false))
    }
  def withReceiver(): Query = copy(hasReceiver = true)
  def withStepSeparator(): Query = copy(hasStepSeparator = true)
}

private[parser] case class QueryHasLeavesSource(
    pre: Option[Query],
    leaves: Seq[Query],
    post: Option[Query],
    ind: Int,
    hasReceiver: Boolean = false,
    hasStepSeparator: Boolean = false
) extends Query {
  def toLines: Seq[String] =
    pre.toSeq.flatMap(_.toLines) :++
      leaves.flatMap(_.toLines) :++
      post.toSeq.flatMap(_.toLines)
  def withReceiver(): Query = copy(
    pre = pre.map(_.withReceiver()),
    hasReceiver = true
  )
  def withStepSeparator(): Query = copy(
    leaves =
      if (post.nonEmpty) leaves
      else
        leaves match {
          case init :+ last => init :+ last.withStepSeparator()
          case l            => l
        },
    post = post.map(_.withStepSeparator()),
    hasStepSeparator = true
  )
}

private[parser] case class QueryHasTwoLeavesSource(
    pre: Option[Query],
    leaves1: Seq[Query],
    mid: Option[Query],
    leaves2: Seq[Query],
    post: Option[Query],
    ind: Int,
    hasReceiver: Boolean = false,
    hasStepSeparator: Boolean = false
) extends Query {
  def toLines: Seq[String] =
    pre.toSeq.flatMap(_.toLines) :++
      leaves1.flatMap(_.toLines) :++
      mid.toSeq.flatMap(_.toLines) :++
      leaves2.flatMap(_.toLines) :++
      post.toSeq.flatMap(_.toLines)
  def withReceiver(): Query = copy(
    pre = pre.map(_.withReceiver()),
    hasReceiver = true
  )
  def withStepSeparator(): Query = copy(
    leaves2 =
      if (post.nonEmpty) leaves2
      else
        leaves2 match {
          case init :+ last => init :+ last.withStepSeparator()
          case l            => l
        },
    post = post.map(_.withStepSeparator()),
    hasStepSeparator = true
  )
}

private[parser] case class QueryNone(
    ind: Int = 0,
    hasReceiver: Boolean = false,
    hasStepSeparator: Boolean = false
) extends Query {
  def toLines: Seq[String] = Nil
  def withReceiver(): Query = copy(hasReceiver = true)
  def withStepSeparator(): Query = copy(hasStepSeparator = true)
}
