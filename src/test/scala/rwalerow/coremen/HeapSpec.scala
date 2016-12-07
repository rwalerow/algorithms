package rwalerow.coremen

import org.scalatest.{Matchers, WordSpec}
import rwalerow.cormen.Heap

/**
  * Created by robert on 02.12.16.
  */
class HeapSpec extends WordSpec with Matchers {

  import rwalerow.utils.CompareImplicits._

  "Max heap" should {
    "build end extract correctly" in {
      val heap = Heap.buildHeap(Array(1,2,3,4,5,6,7), Heap.Max)
      val maxSort = (1 to 7).map(_ => heap.heapExtractMax).toList

      maxSort should contain theSameElementsInOrderAs List(7,6,5,4,3,2,1)
    }

    "build end extract min heap" in {
      val heap = Heap.buildHeap(Array(7,6,5,4,3,2,1), Heap.Min)
      val minSort = (1 to 7).map(_ => heap.heapExtractMax).toList

      minSort should contain theSameElementsInOrderAs List(1,2,3,4,5,6,7)
    }
  }
}
