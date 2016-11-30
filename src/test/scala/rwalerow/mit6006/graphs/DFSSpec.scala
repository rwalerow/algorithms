package rwalerow.mit6006.graphs

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by robert on 30.11.16.
  */
class DFSSpec extends WordSpec with Matchers {

  trait Graphs {
    val vertices = List(1, 2 ,3 ,4 ,5 ,6 ,7 ,8)

    val edgesWithoutCycle = List(1 -> 2, 2 -> 3, 2 -> 4,
      3 -> 4, 3 -> 5, 4 -> 6, 4 -> 8, 7 -> 6, 7 -> 8, 6 -> 8
    )

    val edgesWithCycle = (8 -> 2) :: edgesWithoutCycle

    val graphWithCycle = Graph.createGraph(vertices, edgesWithCycle)
    val graphWithoutCycle = Graph.createGraph(vertices, edgesWithoutCycle)
  }

  "DFS find cycle" should {

    "detect cycle" in new Graphs {
      DFS.detectCycle(graphWithCycle) shouldBe true
    }

    "not detect cycle" in new Graphs {
      DFS.detectCycle(graphWithoutCycle) shouldBe false
    }
  }
}
