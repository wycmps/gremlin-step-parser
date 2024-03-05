package gremlin.step.parser

import com.typesafe.scalalogging.StrictLogging
import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversal
import gremlin.scala.GremlinScala
import scala.util.Try

/** [使い方] import cel.core.script.parser.RichGraphTraversalParser.*
  *
  * val traversal: GraphTraversal[_, E] = ??? traversal.tapPrintTraversal()
  */
object RichGraphTraversalOps extends StrictLogging {

  implicit class RichGraphTraversal[S, E](val underlying: GraphTraversal[S, E])
      extends AnyVal {
    def tapPrintTraversal(): GraphTraversal[S, E] = {
      Try {
        val query = RichGraphTraversalParser.toQueryFrom(underlying.toString)
        logger.info(query)
      } recover { case th: Throwable =>
        logger.warn(s"$underlying", th)
      }
      underlying
    }
  }

  implicit class RichGremlinScala[E](val underlying: GremlinScala[E])
      extends AnyVal {
    def tapPrintTraversal(): GremlinScala[E] = {
      new RichGraphTraversal(underlying.traversal).tapPrintTraversal()
      underlying
    }
  }
}
