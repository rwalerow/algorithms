package rwalerow.mit6006.graphs

import rwalerow.cormen.Heap.{MinMax}
import rwalerow.utils.Compare

import scala.collection.mutable

/**
  * Created by robert on 06.12.16.
  */
class DijkstraHeap[K: Compare](arr: Array[K])(implicit val minMax: MinMax) {

  val elementIndexes = mutable.HashMap[K, Int]().withDefaultValue(-1)
  var size = arr.length
  var heap: Array[K] = arr

  arr.zipWithIndex.foreach{ case (k, ind) => elementIndexes(k) = ind}

  private val heapSize = arr.length
  (heapSize/2 to 0 by -1).foreach{ i =>
    heapify(i)
  }

  private def parent(i: Int): Int = (i - 1) >> 1  // i/2
  private def left(i: Int): Int = ((i + 1) << 1) - 1    // i*2
  private def right(i: Int): Int = (i + 1) << 1

  def heapInsert(key: K) = {
    var i = size - 1
    elementIndexes(key) = size - 1
    while(i > 1 && Compare[K].compare(heap(parent(i)), key) == minMax.o) {
      heap(i) = heap(parent(i))
      elementIndexes(heap(i)) = i
      i = parent(i)
    }
    heap(i) = key
    elementIndexes(key) = i
  }

  def heapify(i: Int): Unit = {
    val l = left(i)
    val r = right(i)
    var largest = 0

    if(l < size && Compare[K].compare(heap(l), heap(i)) == minMax.b)
      largest = l
    else largest = i

    if(r < size && Compare[K].compare(heap(r), heap(largest)) == minMax.b)
      largest = r

    if(largest != i){
      val help = heap(i)
      heap(i) = heap(largest)
      heap(largest) = help

      elementIndexes(heap(i)) = i
      elementIndexes(heap(largest)) = largest

      heapify(largest)
    }
  }

  def extractPick: K = {
    if(heap.length < 1)
      throw new Error("Kopiec pusty")

    val max = heap(0)
    heap(0) = heap(size - 1)
    elementIndexes(heap(0)) = 0
    size -= 1
    heapify(0)
    max
  }

  def elemIndex(elem: K) = elementIndexes(elem)
  def reheapify(elem: K) = heap compose elementIndexes
  def nonEmpty = size > 0
}
