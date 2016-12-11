package rwalerow.mit6006

import rwalerow.utils.Util

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

  def longestCommonSubsequence(x: String, y: String): String = {
    val infinity = 100000
    val memoized = mutable.HashMap[(Int, Int), (Int, String)]()

    implicit val tupleCompF = (x: (Int, String), y: (Int, String)) =>
      if(x._1 >= infinity) 1
      else if(y._1 >= infinity) -1
      else if(x._2.nonEmpty && y._2.nonEmpty && x._2.length == y._2.length) x._1 - y._1
      else if(x._2.nonEmpty && y._2.nonEmpty && x._2.length > y._2.length) -1
      else if(x._2.nonEmpty && y._2.nonEmpty && x._2.length < y._2.length) 1
      else if(x._2.nonEmpty) -1
      else if(y._2.nonEmpty) 1
      else x._1 - y._1

    def dp(xi: Int, yi: Int): (Int, String) = {
      if(memoized.contains((xi, yi))) memoized((xi, yi))
      else {
        var resp = (0, "")
        if (xi < x.length && yi < y.length) {
          resp = Util.min(
            (costOfReplace(xi, yi) + dp(xi + 1, yi + 1)._1, x(xi) + dp(xi + 1, yi + 1)._2),
            (1 + dp(xi, yi + 1)._1, dp(xi, yi + 1)._2),
            (1 + dp(xi + 1, yi)._1, dp(xi + 1, yi)._2)
          )
          memoized.put((xi, yi), resp)
        }
        resp
      }
    }

    def costOfReplace(xi: Int, yi: Int): Int = {
      if(x(xi) == y(yi)) 0
      else infinity
    }

    dp(0, 0)._2
  }
}
