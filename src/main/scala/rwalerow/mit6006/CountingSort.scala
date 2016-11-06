package rwalerow.mit6006

import scala.collection.mutable.ArrayBuffer

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

  def sort[A](arr: Array[ElementWithKey[A]], k: Int): Array[ElementWithKey[A]] = {
    val counting: Array[ArrayBuffer[A]] = Array.fill(k + 1){ ArrayBuffer[A]() }

    arr.foreach(e => counting(e.key).append(e.value))

    val result: Array[ElementWithKey[A]] = Array.ofDim(arr.length)

    var iter = 0

    counting.indices.foreach{ c =>
      if(counting(c).nonEmpty)
       counting(c).foreach { e =>
         result(iter) = ElementWithKey(c, e)
         iter += 1
       }
    }
    result
  }

  case class ElementWithKey[A](key: Int, value: A)
}
