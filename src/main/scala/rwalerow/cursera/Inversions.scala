package rwalerow.cursera

/**
  * Created by robert on 01.11.16.
  */
object Inversions {

  def count(arr: Array[Int]): (Int, Array[Int]) = {
//    println(arr.mkString(","))
    def merge(first: Array[Int], second: Array[Int]): (Int, Array[Int]) = {
      if (first.isEmpty) (0, second)
      else if (second.isEmpty) (0, first)
      else {

        var i = 0
        var j = 0
        var inversions = 0
        val result = Array.fill(arr.length) {0}

        arr.indices.foreach(k =>
          if(i == first.length) {
            result(k) = second(j)
            j += 1
          } else if(j == second.length) {
            result(k) = first(i)
            i += 1rr.indices.foreach(k =>
          if(i == first.length) {
            result(k) = second(j)
            j += 1
          } else if(j == second.length) {
            result(k) = first(i)
            i += 1
          } else if (first(i) < second(j)) {
            result(k) = first(i)
            i += 1
          } else {
            inversions += first.length - i
            result(k) = second(j)
            j += 1
          }
          } else if (first(i) < second(j)) {
            result(k) = first(i)
            i += 1
          } else {
            inversions += first.length - i
            result(k) = second(j)
            j += 1
          }
        )
        (inversions, result)
      }
    }

    if(arr.length <= 1) (0, arr)
    else {
      val (x, xArray) = count(arr.take((arr.length)/2))
      val (y, yArray) = count(arr.drop((arr.length)/2))
      val (z, zArray) = merge(xArray, yArray)

      (x + y + z, zArray)
    }
  }
}
