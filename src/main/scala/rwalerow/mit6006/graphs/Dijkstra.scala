package rwalerow.mit6006.graphs

import rwalerow.mit6006.graphs.Graph.Graph

import scala.collection.mutable
import rwalerow.cormen.Heap.Min
import rwalerow.utils._

import scala.reflect.ClassTag

/**
  * Created by robert on 02.12.16.
  */
object Dijkstra {

  def paths[K: ClassTag](graph: Graph[K], weights: Map[(K, K), Int], start: K): mutable.Map[K, Int] = {

//    val result: mutable.Map[K, Int] = mutable.HashMap[K, Int]().withDefaultValue(Int.MaxValue)
    val result = mutable.HashMap[K, Int]()
    graph.keySet.foreach(result.put(_, Int.MaxValue))
    result(start) = 0

    implicit val min = Min
    implicit object compFor extends Compare[K] {
      override def compare(first: K, second: K): CompareResult = (first, second) match {
        case (f, s) if result(f) < result(s) => Smaller
        case (f, s) if result(f) > result(s) => Grater
        case _ => Equal
      }
    }

    val q = new DijkstraHeap(graph.keys.toArray)

    while(q.nonEmpty) {
      val curr = q.extractPick
      for(end <- graph(curr)) {
          if (result(curr) != Int.MaxValue && result(curr) + weights((curr, end)) < result(end)) {
            result(end) = result(curr) + weights((curr, end))
            q.heapify(q.elemIndex(end))
          }
        }
    }
    result
  }
}
