package rwalerow.cormen

import rwalerow.utils.{Compare, CompareResult, Grater, Smaller}

object Heap {

  trait MinMax {
    def base: CompareResult
    def opposite = List(Smaller, Grater).filter(_ != base).head
    def b = base
    def o = opposite
  }

  object Min extends MinMax {
    def base = Smaller
  }

  object Max extends MinMax {
    def base = Grater
  }

  case class Heap[K: Compare](heap: Array[K], var size: Int, minMax: MinMax) {

    def heapExtractMax: K = {
      if(heap.length < 1)
        throw new Error("Kopiec pusty")

      val max = heap(0)
      heap(0) = heap(size - 1)
      size -= 1
      heapify(this, 0)
      max
    }

    def heapInsert(key: K) = {
      var i = size - 1
      while(i > 1 && Compare[K].compare(heap(parent(i)), key) == minMax.o) {
        heap(i) = heap(parent(i))
        i = parent(i)
      }
      heap(i) = key
    }
  }

  private def parent(i: Int): Int = (i - 1) >> 1  // i/2
  private def left(i: Int): Int = ((i + 1) << 1) - 1    // i*2
  private def right(i: Int): Int = (i + 1) << 1

  def heapify[K: Compare](heapImpl: Heap[K], i: Int): Unit = {
    val l = left(i)
    val r = right(i)
    val heapSize = heapImpl.size
    val heap = heapImpl.heap
    var largest = 0

    if(l <= heapSize && Compare[K].compare(heap(l), heap(i)) == heapImpl.minMax.b)
      largest = l
    else largest = i

    if(r <= heapSize && Compare[K].compare(heap(r), heap(largest)) == heapImpl.minMax.b)
      largest = r

    if(largest != i){
      val help = heap(i)
      heap(i) = heap(largest)
      heap(largest) = help
      heapify(heapImpl, largest)
    }
  }

  def buildHeap[K: Compare](heap: Array[K], minMax: MinMax): Heap[K] = {
    val heapSize = heap.length
    (heapSize/2 to 0 by -1).foreach{ i =>
      heapify(Heap(heap, heap.length - 1, minMax), i)
    }
    Heap(heap, heap.length, minMax)
  }

  def sort[K: Compare](heap: Array[K], minMax: MinMax): Unit = {
    buildHeap(heap, minMax)
    ((heap.length - 1) to 1 by -1).foreach{ i =>
      val help = heap(0)
      heap(0) = heap(i)
      heap(i) = help
      heapify(Heap(heap, i - 1, minMax), 0)
    }
  }
}