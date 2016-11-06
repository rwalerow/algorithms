package rwalerow.mit6006

import org.scalatest.{FlatSpec, Matchers}
import rwalerow.mit6006.CountingSort.ElementWithKey

/**
  * Created by robert on 06.11.16.
  */
class RadixSortSpec extends FlatSpec with Matchers {

  "Radix" should "sort array with big numbers" in {
    val arr = Array(
      ElementWithKey(3241, "a"), ElementWithKey(5234, "o"),
      ElementWithKey(13, "z"), ElementWithKey(351, "w"),
      ElementWithKey(4511, "p"), ElementWithKey(353, "a")
    )

    val expected = Array(
      ElementWithKey(13, "z"), ElementWithKey(351, "w"),
      ElementWithKey(353, "a"), ElementWithKey(3241, "a"),
      ElementWithKey(4511, "p"), ElementWithKey(5234, "o")
    )

    val result = RadixSort.sort(arr)

    result should contain theSameElementsInOrderAs expected
  }

}
