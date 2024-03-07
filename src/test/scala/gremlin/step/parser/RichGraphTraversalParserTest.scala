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
    // println(queryAsTry.get)
  }

  test("case2") {
    val ss = """
     |[GraphStep(vertex,[v[336117792], v[665055320], v[819986456], v[1034350728], v[98648296], v[118169736], v[122900488], v[167350408], v[167673968], v[180912344], v[229093608], v[260620520], v[263241968]]),
     | TraversalFilterStep([
     |   AndStep([
     |     [VertexStep(IN,[element_of_textbook_group],edge), HasStep([grade_subject_code.eq(c)])],
     |     [VertexStep(IN,[child_structure_elements],vertex), HasStep([data_type.eq(textbook_type_qubena)])]
     |   ])
     | ]),
     | TraversalFilterStep([
     |   RepeatStep(
     |     [VertexStep(IN,[child_structure_elements, program_in_structure_element],edge), EdgeVertexStep(OUT), RepeatEndStep],
     |     until([HasStep([~label.eq(structure_element), data_type.within([textbook_type_qubena])])]),
     |     emit(false)
     |   )
     | ]),
     | OrderGlobalStep([
     |   [[
     |      VertexStep(IN,[child_structure_elements],vertex),
     |      VertexStep(IN,[child_structure_elements],vertex),
     |      VertexStep(IN,[child_structure_elements],edge),
     |      PropertiesStep([order],value)
     |    ], asc
     |   ]
     | ])
     |]""".stripMargin
    val queryAsTry = RichGraphTraversalParser.toQueryAsTryFrom(ss)
    queryAsTry.failed.foreach(th => th.printStackTrace())
    assert(queryAsTry.isSuccess)
    println(queryAsTry.get)
  }
}
