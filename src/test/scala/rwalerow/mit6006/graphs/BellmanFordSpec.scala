package rwalerow.mit6006.graphs

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by robert on 07.12.16.
  */
class BellmanFordSpec extends WordSpec with Matchers {

  "Bellman-ford" should {
    "find the same as Dijkstra form 2" in new DirectedGraph {
      val result = BellmanFord.paths(g, weights, 2).right.get
      val expectedResult = Map(1 -> Int.MaxValue, 2 -> 0, 3 -> Int.MaxValue,
        4 -> Int.MaxValue, 5 -> 3, 6 -> 6, 7 -> 5, 8 -> 5, 9 -> 4, 10 -> 8
      )

      result should contain theSameElementsAs expectedResult
    }

    "detect negative cycle" in new DirectedGraph {
      val result = BellmanFord.paths(g, weights ++ List(8 -> 10 -> -8), 2)
      result shouldBe Left("Negative cycle detected")
    }
  }

}
