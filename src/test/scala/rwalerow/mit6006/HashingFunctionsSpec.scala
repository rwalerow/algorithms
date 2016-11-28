package rwalerow.mit6006

import org.scalatest.{FlatSpec, Matchers, WordSpec}
import rwalerow.mit6006.HashingFunctions._
/**
  * Created by robert on 27.11.16.
  */
class HashingFunctionsSpec extends WordSpec with Matchers {

  "Summing hash" should {

    "equal naive implementation for single char" in {
      val t = "a"
      val naive = hash(t, 97) % 997
      val regular = summingHash(t, 97, 997)

      naive shouldEqual regular
    }

    "equal naive implementation for 'ar' char" in {
      val t = "ar"
      val naive = hash(t, 97) % 997
      val regular = summingHash(t, 97, 997)

      naive shouldEqual regular
    }

    "equal naive implementation for robert" in {
      val testString = "robert"
      val naive = hash(testString, 97) % 997
      val regular = summingHash(testString, 97, 997)

      naive shouldEqual regular
    }
  }
}
