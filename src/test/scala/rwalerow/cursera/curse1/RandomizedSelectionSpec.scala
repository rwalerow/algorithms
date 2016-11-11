package rwalerow.cursera.curse1

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by robert on 07.11.16.
  */
class RandomizedSelectionSpec extends FlatSpec with Matchers {

  "Randomized" should "find solution with odd number of elements" in {
    val array = Array(4,5,7,2,44,85,32,15,33)

    val result = RandomizedSelection.find(array, array.length/2)

    result shouldBe 15
  }

  it should "find first element" in {
    val array = Array(4,5,7,2,44,85,32,15,33)

    val result = RandomizedSelection.find(array, 0)

    result shouldBe 2
  }
}
