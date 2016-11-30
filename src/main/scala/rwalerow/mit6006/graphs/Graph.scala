package rwalerow.mit6006.graphs

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by robert on 29.11.16.
  */
object Graph {

  type Graph[K] = mutable.HashMap[K, ListBuffer[K]]

  def createGraph[K](vertices: Iterable[K], edges: Iterable[(K, K)]): Graph[K] = {
    val result = mutable.HashMap.empty[K, ListBuffer[K]]
    vertices.foreach(result.put(_, ListBuffer()))
    for ((start, finish) <- edges){
      if(result.keySet.contains(start)) result(start).append(finish)
      else result.put(start, ListBuffer(finish))
    }
    result
  }
}
