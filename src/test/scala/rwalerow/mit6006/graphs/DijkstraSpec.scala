package rwalerow.mit6006.graphs

import org.scalatest.{Matchers, WordSpec}

import scala.collection.mutable
import Int.MaxValue

trait DirectedGraph {
  val vertices = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val weights = Map( 1 -> 3 -> 1, 2 -> 5 -> 3, 10 -> 5 -> 5, 10 -> 9 -> 1, 1 -> 4 -> 3,
    3 -> 6 -> 3, 5 -> 8 -> 2, 5 -> 7 -> 2, 4 -> 6 -> 3, 4 -> 7 -> 6, 9 -> 6 -> 2, 8 -> 9 -> 3,
    5 -> 9 -> 1, 2 -> 10 -> 8
  )

  val g = Graph.createGraph(vertices, weights.keySet)
}

class DijkstraSpec extends WordSpec with Matchers {

  "Dijkstra" should {
    "find length from 2" in new DirectedGraph {
      val result = Dijkstra.paths(g, weights, 2)
      val expectedResult = Map(1 -> Int.MaxValue, 2 -> 0, 3 -> Int.MaxValue,
        4 -> Int.MaxValue, 5 -> 3, 6 -> 6, 7 -> 5, 8 -> 5, 9 -> 4, 10 -> 8
      )

      result should contain theSameElementsAs expectedResult
    }

    "find length from 1" in new DirectedGraph {
      val result = Dijkstra.paths(g, weights, 1)
      val expectedResult = Map(1 -> 0, 2 -> Int.MaxValue, 3 -> 1, 4 -> 3, 5 -> Int.MaxValue,
        6 -> 4, 7 -> 9, 8 -> Int.MaxValue, 9 -> Int.MaxValue, 10 -> Int.MaxValue
      )

      result should contain theSameElementsAs expectedResult
    }
  }
}
