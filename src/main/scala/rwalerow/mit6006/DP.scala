package rwalerow.mit6006

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Dynamic Programming
  */
object DP {

  def longestSubsequence(seq: Array[Int]): List[Int] = {

    val answers = mutable.HashMap[Int, (Int, List[Int])]()

    for(i <- (seq.length - 1) to 0 by -1) {
      val choises: ListBuffer[(Int, List[Int])] = ListBuffer(1 -> List(seq(i)))
      for(j <- (i+1) until seq.length) {
        if(seq(i) < seq(j) && i != j) {
          choises += (answers(j)._1 + 1 -> (seq(i) :: answers(j)._2))
        }
      }
      answers(i) = choises.maxBy{ case (key, value) => key}
    }

    answers.maxBy{ case (_, (length, _)) => length }._2._2
  }
}
