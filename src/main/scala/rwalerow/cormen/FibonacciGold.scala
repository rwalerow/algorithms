package rwalerow.cormen

/**
  * Created by robert on 31.10.16.
  */
object FibonacciGold {
  def fibonacci(i: Int): Int = {
    val o = (1 + Math.sqrt(5.0))/2.0
    val os = (1 - Math.sqrt(5.0))/2.0

    ((Math.pow(o, i) - Math.pow(os, i))/Math.sqrt(5)).toInt
  }
}
