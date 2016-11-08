package rwalerow.cursera

/**
  * Created by robert on 07.11.16.
  */
object RandomizedSelection {

  def find(arr: Array[Int], i: Int): Int = {
    def findInner(arr: Array[Int], left: Int, right: Int): Int = {
      val partition = QuickSort.partition(arr, left, right)

      if(partition == i) arr(i)
      else if(partition < i ) findInner(arr, partition + 1, right)
      else findInner(arr, left, partition - 1)
    }

    findInner(arr, 0, arr.length - 1)
  }
}
