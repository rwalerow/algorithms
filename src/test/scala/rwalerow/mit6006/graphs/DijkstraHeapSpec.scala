package rwalerow.mit6006.graphs

import org.scalatest.{Matchers, WordSpec}
import rwalerow.cormen.Heap.Min
import rwalerow.utils._

/**
  * Created by robert on 06.12.16.
  */
class DijkstraHeapSpec extends WordSpec with Matchers {

  "Dijkstra heap" should {

    "properly index elements" in {
      val data = Map(("a", 7), ("b", 6), ("c", 5), ("d", 4),
        ("e", 3), ("f", 2), ("g", 1))

      implicit val min = Min
      implicit object outsideComp extends Compare[String] {
        override def compare(first: String, second: String): CompareResult = (first, second) match {
          case (f, s) if data(f) < data(s) => Smaller
          case (f, s) if data(f) == data(s) => Equal
          case _ => Grater
        }
      }

      val heap = new DijkstraHeap[String](data.keySet.toArray)

      heap.heap.foreach{ x =>
        (heap.heap compose heap.elementIndexes)(x) shouldBe x
      }

      heap.extractPick shouldBe "g"
      heap.extractPick shouldBe "f"
      heap.extractPick shouldBe "e"
      heap.extractPick shouldBe "d"
      heap.extractPick shouldBe "c"
      heap.extractPick shouldBe "b"
      heap.extractPick shouldBe "a"

    }
  }
}
