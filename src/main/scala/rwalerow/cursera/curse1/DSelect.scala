package rwalerow.cursera.curse1

/**
  * Created by robert on 07.11.16.
  */
object DSelect {

  def choosePivotMedian(arr: Array[Int])(l: Int, r: Int): Int = {
    if(arr.length < 6) arr.indexOf(QuickSort.sort(arr)((l - r)/2))
    else {
      val parts = arr.slice(l, r).sliding(5, 5).map(QuickSort.sort).toArray
      val roundCandidates = parts.map(l => l(l.length / 2))
      arr.indexOf(find(roundCandidates, roundCandidates.length / 2))
    }
  }

  def find(arr: Array[Int], i: Int): Int = {

    def findInner(arr: Array[Int], left: Int, right: Int): Int = {
      val partition = QuickSort.partition(arr, left, right, choosePivotMedian(arr))

      if(partition == i) arr(i)
      else if(partition < i ) findInner(arr, partition + 1, right)
      else findInner(arr, left, partition - 1)
    }

    findInner(arr, 0, arr.length - 1)
  }
}
