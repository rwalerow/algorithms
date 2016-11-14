package rwalerow.cursera.curse2

import org.scalatest.enablers.Aggregating
import org.scalatest.{FlatSpec, Matchers}
import rwalerow.cursera.curse2.Graph.Vertex

/**
  * Created by robert on 14.11.16.
  */
class KosarajuSpec extends FlatSpec with Matchers {

  "Kosaraju" should "find 3 groups" in {
    val edges = List((1, 2), (2, 3), (3,1),
      (3, 4), (4, 6), (5, 6), (6, 7), (7, 5))
    val graph = Graph.createSpecialGraph((1 to 7).toList, edges)

    val first = graph.vertices.filter(x => List(1,2,3).contains(x.key)).toSet
    val second = graph.vertices.filter(x => List(5,6,7).contains(x.key)).toSet
    val third = graph.vertices.filter(x => x.key == 4).toSet

    val espectedResult = List(Set())

    val kosarajuResult = Kosaraju.findStronglyConnected(graph).map(s => s.map(_.key))

    kosarajuResult should contain theSameElementsAs List(first, second, third).map(s => s.map(_.key))
  }
}
