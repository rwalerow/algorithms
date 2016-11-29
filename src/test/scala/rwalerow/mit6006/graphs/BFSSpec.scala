package rwalerow.mit6006.graphs

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by robert on 29.11.16.
  */
class BFSSpec extends WordSpec with Matchers {

  trait Graph {
    val edges = List((1, 2), (2, 1), (1, 4), (4, 1), (2, 4), (4, 2),
      (4, 7), (7, 4), (7, 8), (8, 7), (5, 8), (8, 5), (2, 5), (5, 2),
      (2, 3), (3, 2), (3, 6), (6, 3)
    )
    val fullGraph = Graph.createGraph(edges)
  }

  "BFS" should {
    "find all depths" in new Graph {
      val pathsFor2 = BFS.fromTo(2, fullGraph)

      val expectedResults = Map(
        1 -> 1, 4 -> 1, 7 -> 2, 2 -> 0,
        3 -> 1, 6 -> 2, 5 -> 1, 8 -> 2
      )

      pathsFor2.foreach { case (k, pathLength) =>
        pathLength shouldBe expectedResults(k)
      }
    }
  }
}
