package rwalerow.mit6006

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by robert on 06.11.16.
  */
class CountingSortSpec extends FlatSpec with Matchers {

  "Basic sort" should "sort elements" in {
    val arr = Array(3,4,5,2,45,6,2,3,52,1)

    val result = CountingSort.sort(arr, 52)

    result should contain theSameElementsInOrderAs Array(1,2,2,3,3,4,5,6,45,52)
  }
}
