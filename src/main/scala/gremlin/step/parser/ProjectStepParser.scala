package gremlin.step.parser

import scala.util.chaining._

private[parser] trait ProjectStepParser extends BasicParser {

  private case class ProjectStep(elems: Seq[QElemBy]) extends QStep {
    def toQuery(ind: Int): Query =
      Seq
        .newBuilder[String]
        .addOne(elems.map(_.toQuery).mkString(".project(", ",", ")"))
        .addAll(elems.map(_.toBy))
        .result()
        .pipe(ss => QueryRaws(ss, ind))
  }

  private trait QElemBy extends QElem {
    def toBy: String
  }
  private case class QElemByRaw(override val rawString: String)
      extends QElemBy {
    def toQuery: String = s""""$rawString""""
    def toBy: String =
      if (rawString == "identity") ".by()" else s".by($toQuery)"
  }

  private case class QElemByLambdaMap(override val rawString: String)
      extends QElemBy {
    def toQuery: String = s""""$rawString""""
    def toBy: String = s".by(TODO)" // TODO, データが見つかったら対応
  }

  private def uuids: Parser[Seq[String]] = "[" ~> repsep(uuid, ",") <~ "]"

  private def byValue: Parser[QElemBy] =
    ("identity" | "label") ^^ (s => QElemByRaw(s)) |
      "value(" ~> word <~ ")" ^^ (w => QElemByRaw(w.rawString)) |
      "[PropertiesStep([" ~> word <~ "],property), LambdaMapStep(lambda)]" ^^ (
        e => QElemByLambdaMap(e.rawString)
      )

  private def bys: Parser[Seq[QElemBy]] =
    "[" ~> repsep(byValue, ",") <~ "]"

  def projectStep: Parser[QStep] =
    "ProjectStep(" ~> uuids ~ "," ~ bys <~ ")" ^^ { case _ ~ _ ~ _bys =>
      ProjectStep(_bys)
    }
}
