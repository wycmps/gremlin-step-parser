package gremlin.step.parser

import scala.util.Try
import scala.util.parsing.combinator._

/** GraphTraversalのStepsからGraphDBのクエリを構築するパーサー
  */
object RichGraphTraversalParser
    extends RegexParsers
    with GraphStepParser
    with HasStepParser
    with VertexStepParser
    with EdgeVertexStepParser
    with TraversalFilterStepParser
    with RepeatStepParser
    with OrderGlobalStepParser
    with SubgraphStepParser
    with RepeatEndStepParser
    with SideEffectCapStepParser
    with GroupStepParser
    with PropertiesStepParser
    with FoldStepParser
    with ProjectStepParser
    with LambdaMapStepParser
    with InjectStepParser
    with OrStepParser
    with SelectOneStepParser
    with TraversalMapStepParser
    with UnionStepParser
    with EndStepParser
    with PathFilterStepParser
    with PathStepParser
    with UnfoldStepParser
    with AndStepParser {

  private def traversalFilterStep: Parser[QStep] = traversalFilterStepDelegate(
    stepParser
  )
  private def orderGlobalStep: Parser[QStep] = orderGlobalStepDelegate(
    stepParser
  )
  private def repeatStep: Parser[QStep] = repeatStepDelegate(stepParser)
  private def groupStep: Parser[QStep] = groupStepDelegate(stepParser)
  private def orStep: Parser[QStep] = orStepDelegate(stepParser)
  private def traversalMapStep: Parser[QStep] = traversalMapStepDelegate(stepParser)
  private def unionStep: Parser[QStep] = unionStepDelegate(stepParser)
  private def andStep: Parser[QStep] = andStepDelegate(stepParser)

  private def stepParser: Parser[QStep] =
    graphStep | hasStep | vertexStep | traversalFilterStep |
      edgeVertexStep | repeatStep | orderGlobalStep | subgraphStep |
      repeatEndStep | sideEffectCapStep | groupStep | propertiesStep |
      foldStep | projectStep | lambdaMapStep | injectStep |
      orStep | selectOneStep | traversalMapStep | unionStep | endStep |
      pathFilterStep | pathStep | unfoldStep | andStep

  def root: Parser[Seq[QStep]] =
    "[" ~> repsep(stepParser, ",") <~ "]" ^^ (_steps => _steps)

  private def parseStepsAsTry(input: String): Try[Seq[QStep]] =
    parseAll(root, input) match {
      case Success(result, _) => scala.util.Success(result)
      case failure: NoSuccess =>
        scala.util.Failure(
          new RuntimeException(s"Failed to parse input: $failure")
        )
    }

  private def toQueriesFrom(steps: Seq[QStep]): Seq[Query] =
    steps match {
      case head :: tail => head.toQuery(0) +: tail.map(_.toQuery(1))
      case Nil          => Nil
    }

  def toQueryAsTryFrom(input: String): Try[String] = {
    parseStepsAsTry(input).map { steps =>
      toQueriesFrom(steps)
        .flatMap(_.toLines)
        .mkString("")
    }
  }

  def toQueryFrom(input: String): String = toQueryAsTryFrom(input).get
}
