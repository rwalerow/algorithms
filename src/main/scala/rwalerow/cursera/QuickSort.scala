package rwalerow.cursera

import scala.util.Random

/**
  * Created by robert on 06.11.16.
  */
object QuickSort {

  class ExtendedArray[A](a: Array[A]) {
    def swap(f: Int, s: Int): Unit = {
      val tmp = a(f)
      a(f) = a(s)
      a(s) = tmp
    }
  }

  implicit def extendedArray[A](array: Array[A]): ExtendedArray[A] = new ExtendedArray[A](array)

  def choosePivot(from: Int, to: Int): Int = Random.nextInt(to - from + 1) + from

  def partition(arr: Array[Int], l: Int, r: Int,
                choosePivotF: (Int, Int) => Int = choosePivot): Int = {
    arr.swap(choosePivotF(l, r), l)
    val p = arr(l)
    var i = l + 1
    for(j <- l + 1 to r) {
      if(arr(j) < p) {
        arr.swap(j, i)
        i += 1
      }
    }
    arr.swap(i - 1, l)
    i - 1
  }

  def sort(arr: Array[Int]): Array[Int] = {

    def innerSort(arr: Array[Int], l: Int, r: Int): Unit = {
      if(r - l > 0){
        val pivotPlace = partition(arr, l, r)
        innerSort(arr, l, pivotPlace - 1)
        innerSort(arr, pivotPlace + 1, r)
      }
    }

    val clone = arr.clone()
    innerSort(clone, 0, arr.length - 1)
    clone
  }
}
