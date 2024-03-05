package gremlin.step.parser

private[parser] trait HasStepParser extends BasicParser {

  sealed private trait OP {
    def toQuery: String
  }

  private case class HasLabelKeyEqVal(label: QElem, key: QElem, value: QElem)
      extends OP {
    def toQuery: String =
      s""".has(${label.toQuery}, ${key.toQuery}, ${value.toQuery})"""
  }

  private case class HasLabelVal(label: QElem, value: QElem) extends OP {
    def toQuery: String =
      s""".has(${label.toQuery}, ${value.toQuery})"""
  }

  private case class HasLabel(label: QElem) extends OP {
    def toQuery: String = s""".hasLabel(${label.toQuery})"""
  }

  private case class HasId(id: QElem) extends OP {
    def toQuery: String = s""".hasId(${id.toQuery})"""
  }

  private case class Within(
      label: QElem,
      keyOpt: Option[QElem],
      vals: Seq[QElem]
  ) extends OP {
    def toQuery: String =
      keyOpt
        .map { key =>
          s""".has(${label.toQuery}, ${key.toQuery}, within(${vals
              .map(_.toQuery)
              .mkString(", ")}))"""
        }
        .getOrElse(s""".has(${label.toQuery}, within(${vals
            .map(_.toQuery)
            .mkString(", ")}))""")
  }

  private case class GT(label: QElem, v: QElem) extends OP {
    def toQuery: String = s""".has(${label.toQuery}, gt(${v.toQuery}))"""
  }

  private case class HasStep(op: OP, aliasOpt: Option[QElem]) extends QStep {
    def toQuery(ind: Int): Query = QueryRaw(
      s"${op.toQuery}${aliasOpt.fold("")(a => s".as(${a.toQuery})")}",
      ind
    )
  }

  private def label: Parser[QElem] = "~label.eq(" ~> word <~ ")"
  private def id: Parser[QElem] = "~id.eq(" ~> number <~ ")"
  private def within: Parser[Seq[QElem]] =
    ".within([" ~> repsep(number | word, ",") <~ "])"

  private def labelWithin: Parser[OP] = word ~ within ^^ { case label ~ elems =>
    Within(label, None, elems)
  }
  private def labelGtVal: Parser[OP] =
    word ~ ".gt(" ~ (number | word) <~ ")" ^^ { case label ~ _ ~ value =>
      GT(label, value)
    }

  private def labelKeyEqVal: Parser[OP] =
    label ~ "," ~ word ~ ".eq(" ~ word <~ ")" ^^ {
      case label ~ _ ~ key ~ _ ~ value => HasLabelKeyEqVal(label, key, value)
    }

  private def labelEqVal: Parser[OP] =
    word ~ ".eq(" ~ word <~ ")" ^^ { case key ~ _ ~ value =>
      HasLabelVal(key, value)
    }

  private def labelWithinWithKey: Parser[OP] = label ~ "," ~ word ~ within ^^ {
    case label ~ _ ~ key ~ elems => Within(label, Some(key), elems)
  }
  private def hasLabel: Parser[OP] = label ^^ (label => HasLabel(label))
  private def hasId: Parser[OP] = id ^^ (id => HasId(id))

  private def arg3Op: Parser[OP] = labelWithinWithKey
  private def arg2Op: Parser[OP] = labelWithin | labelKeyEqVal
  private def arg1Op: Parser[OP] = labelEqVal | labelGtVal | hasLabel | hasId
  private def op: Parser[OP] = arg3Op | arg2Op | arg1Op

  def hasStep: Parser[QStep] =
    "HasStep([" ~> op ~ "])" ~ opt("@[" ~> word <~ "]") ^^ {
      case _op ~ _ ~ alias => HasStep(_op, alias)
    }
}
