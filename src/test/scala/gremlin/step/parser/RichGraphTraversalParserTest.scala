package gremlin.step.parser

import com.typesafe.scalalogging.StrictLogging
import munit._

class RichGraphTraversalParserTest extends FunSuite with StrictLogging {

  test("case1") {
    val ss = """
      |[GraphStep(vertex,[]),
      | HasStep([~label.eq(structure_element), data_code.eq(tshR03)]),
      | VertexStep(OUT,vertex),
      | HasStep([~label.eq(structure_element), data_type.eq(textbook_type_qubena)]),
      | VertexStep(OUT,vertex),
      | HasStep([~label.eq(structure_element), data_type.eq(grade)]),
      | TraversalFilterStep([
      |   VertexStep(IN,[element_of_textbook_group],edge),
      |   HasStep([grade_subject_code.eq(a)]),
      |   EdgeVertexStep(OUT),
      |   HasStep([~label.eq(textbook_group), data_code.eq(tshR03)])
      | ])
      |]""".stripMargin
    val queryAsTry = RichGraphTraversalParser.toQueryAsTryFrom(ss)
    queryAsTry.failed.foreach(th => th.printStackTrace())
    assert(queryAsTry.isSuccess)
    println(queryAsTry.get)
  }
}
