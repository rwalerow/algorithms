package rwalerow.cursera

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by robert on 07.11.16.
  */
class DSelectSpec extends FlatSpec with Matchers {

  "DSelect" should "find solution with odd number of elements" in {
    val array = Array(4,5,7,2,44,85,32,15,33)

    val result = DSelect.find(array, array.length/2)

    result shouldBe 15
  }

  it should "find first element" in {
    val array = Array(4,5,7,2,44,85,32,15,33)

    val result = DSelect.find(array, 0)

    result shouldBe 2
  }

  it should "work for 2 element list" in {
    val array = Array(1,2)

    val result = DSelect.find(array, 0)

    result shouldBe 1
  }
}
