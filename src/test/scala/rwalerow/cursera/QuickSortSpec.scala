package rwalerow.cursera

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by robert on 06.11.16.
  */
class QuickSortSpec extends FlatSpec with Matchers {

  "Quicksort" should "properly sort basic array" in {
    val arr = Array(5,3,4,61,6,17,62,345,76)
    val expected = Array(3,4,5,6,17,61,62,76,345)

    val result = QuickSort.sort(arr)

    result should contain theSameElementsInOrderAs expected
  }

  it should "sort 2 elements array" in {
    val arr = Array(5, 4)

    val result = QuickSort.sort(arr)

    result should contain theSameElementsInOrderAs(Array(4, 5))
  }
}
