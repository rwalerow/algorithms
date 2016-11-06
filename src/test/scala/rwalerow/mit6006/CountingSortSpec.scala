package rwalerow.mit6006

import org.scalatest.{FlatSpec, Matchers}
import rwalerow.mit6006.CountingSort.ElementWithKey

/**
  * Created by robert on 06.11.16.
  */
class CountingSortSpec extends FlatSpec with Matchers {

  "Basic sort" should "sort elements" in {
    val arr = Array(3,4,5,2,45,6,2,3,52,1)

    val result = CountingSort.sort(arr, 52)

    result should contain theSameElementsInOrderAs Array(1,2,2,3,3,4,5,6,45,52)
  }

  "Sort" should "include additional data" in {
    val arr = Array(
      ElementWithKey(5, "a"), ElementWithKey(9, "o"),
      ElementWithKey(2, "z"), ElementWithKey(5, "w"),
      ElementWithKey(4, "p"), ElementWithKey(2, "a")
    )

    val expected = Array(
      ElementWithKey(2, "z"), ElementWithKey(2, "a"),
      ElementWithKey(4, "p"), ElementWithKey(5, "a"),
      ElementWithKey(5, "w"), ElementWithKey(9, "o")
    )

    val result = CountingSort.sort(arr, 9)

    result should contain theSameElementsInOrderAs expected
  }
}
