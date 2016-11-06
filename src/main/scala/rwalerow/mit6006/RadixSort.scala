package rwalerow.mit6006
import CountingSort._
/**
  * Created by robert on 06.11.16.
  */
object RadixSort {

  def sort[A](array: Array[ElementWithKey[A]]): Array[ElementWithKey[A]] = {

    val k = 10 // k value should be O(n)
    val d = 4 // digits
    var result = array

    (0 until d).foreach(i => {
      val f: Int => Int = n => (n / Math.pow(k, i)).toInt % k
      result = CountingSort.sort2(result, k, f)
    })

    result
  }
}
