package rwalerow.cursera.curse2
import rwalerow.cursera.curse2.Graph.{Edge, Graph, Vertex}

import scala.collection.mutable

object BFS {

  def depth(start: Vertex[Int])(end: Vertex[Int]): Int = {

    if(start eq end) return 0
    val leftEdges: mutable.Queue[Edge[Int]] = mutable.Queue()
    leftEdges.enqueue(start.edges:_*)
    start.visited = true

    while(leftEdges.nonEmpty) {
      val edge = leftEdges.dequeue()
      val endV = edge.end
      if(!endV.visited) {
        endV.visited = true
        endV.depth = edge.start.depth + 1
      }
      if(endV.key == end.key) return endV.depth

      val l = endV.edges
        .filterNot(_.end.visited)

      leftEdges.enqueue(l:_*)
    }
    0
  }
}
