package rwalerow.mit6006.graphs

import rwalerow.mit6006.graphs.Graph.Graph

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * Created by robert on 07.12.16.
  */
object BellmanFord {

  def paths[K: ClassTag](graph: Graph[K], weights: Map[(K, K), Int], start: K): Either[String, Map[K, Int]] = {
    val result = mutable.HashMap[K, Int]()
    graph.keySet.foreach(result.put(_, Int.MaxValue))
    result(start) = 0

    for(_ <- 0 until graph.keySet.size){
      for(((start, end), weight) <- weights){
        if(result(start) != Int.MaxValue && result(end) > result(start) + weight){
          result(end) = result(start) + weight
        }
      }
    }

    for(((start, end), weight) <- weights){
      if(result(start) != Int.MaxValue && result(end) > result(start) + weight){
        return Left("Negative cycle detected")
      }
    }

    Right(result.toMap)
  }
}
