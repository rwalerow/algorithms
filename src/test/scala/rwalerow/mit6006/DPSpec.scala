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

  "Longest common subsequence" should {

    "find int the same words" in {
      val word = "mama"

      DP.longestCommonSubsequence(word, word) shouldBe "mama"
    }

    "find for different length" in {
      val first = "ala"
      val second = "al"

      DP.longestCommonSubsequence(first, second) shouldBe "al"
    }

    "find split subsequence" in {
      val first = "alo"
      val second = "awl"

      DP.longestCommonSubsequence(first, second) shouldBe "al"
    }

    "find split subsequence 2" in {
      val first = "alo"
      val second = "awl"

      DP.longestCommonSubsequence(second, first) shouldBe "al"
    }

    "find split subsequence 3" in {
      val first = "arorl"
      val second = "wlal"

      DP.longestCommonSubsequence(first, second) shouldBe "al"
    }

    "find same letters" in {
      val first = "alwla"
      val second = "ltsl"

      DP.longestCommonSubsequence(first, second) shouldBe "ll"
    }

    "find hello" in {
      val first = "hieroglyphology"
      val second = "michaelangelo"

      List(DP.longestCommonSubsequence(first, second)) should contain oneOf ("hello", "heglo")
    }
  }
}
