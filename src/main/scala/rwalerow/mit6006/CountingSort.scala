package rwalerow.mit6006

/**
  * Created by robert on 06.11.16.
  */
object CountingSort {

  def sort(arr: Array[Int], k: Int): Array[Int] = {
    val counting = Array.fill(k + 1){0}

    arr.foreach(e => counting(e) = counting(e) + 1)
    val result = Array.fill(arr.length){0}

    var iter = 0
    counting.indices.foreach{ c =>
      if(counting(c) != 0){
        (0 until counting(c)).foreach{_ => result(iter) = c; iter += 1}
      }
    }

    result
  }
}
