package rwalerow.cormen

object Heap {

  case class Heap(heap: Array[Int], var size: Int)

  private def parent(i: Int): Int = (i - 1) >> 1  // i/2
  private def left(i: Int): Int = ((i + 1) << 1) - 1    // i*2
  private def right(i: Int): Int = ((i + 1) << 1)

  private def heapify(heapImpl: Heap, i: Int): Unit = {
    val l = left(i)
    val r = right(i)
    val heapSize = heapImpl.size
    val heap = heapImpl.heap
    var largest = 0

    if(l <= heapSize && heap(l) > heap(i))
      largest = l
    else largest = i

    if(r <= heapSize && heap(r) > heap(largest))
      largest = r

    if(largest != i){
      val help = heap(i)
      heap(i) = heap(largest)
      heap(largest) = help
      heapify(heapImpl, largest)
    }
  }

  private def buildHeap(heap: Array[Int]): Unit = {
    val heapSize = heap.length
    (heapSize/2 to 0 by -1).foreach{ i =>
      heapify(Heap(heap, heap.length - 1), i)
    }
  }

  def sort(heap: Array[Int]): Unit = {
    buildHeap(heap)
    ((heap.length - 1) to 1 by -1).foreach{ i =>
      val help = heap(0)
      heap(0) = heap(i)
      heap(i) = help
      heapify(Heap(heap, i - 1), 0)
    }
  }

//  def heapExractMax(heap: Array[Int]): Int = {
//
//  }
}