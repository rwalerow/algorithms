package rwalerow.mit6006

import org.scalatest.{Matchers, WordSpec}

/**
  * Created by robert on 10.12.16.
  */
class DPSpec extends WordSpec with Matchers {

  "Longest Subsequence" should {
    "find longes 1" in {
      val input = Array(9, 3, 2, 5, 7, 6, 1, 4, 8)
      val result = DP.longestSubsequence(input)
      List(result) should contain oneOf (List(3, 5, 6, 8), List(2, 5, 6, 8), List(2, 5, 7, 8))
    }

    "find longes 2" in {
      val input = Array(8, 7, 3, 6, 4, 5, 9, 1, 2)
      val result = DP.longestSubsequence(input)
      result shouldBe List(3, 4, 5, 9)
    }
  }
}
