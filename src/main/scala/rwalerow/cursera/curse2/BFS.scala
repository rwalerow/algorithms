package rwalerow.cursera.curse2
import rwalerow.cursera.curse2.Graph.{Edge, Graph, Vertex}

import scala.collection.mutable

object BFS {

  def depth(start: Vertex[Int])(end: Vertex[Int]): Int = {
    if(start eq end) return 0

    val leftEdges: mutable.Queue[Edge[Int]] = mutable.Queue()
    val depths = mutable.HashMap(start -> 0)

    leftEdges.enqueue(start.edges:_*)
    start.visited = true

    while(leftEdges.nonEmpty) {
      val edge = leftEdges.dequeue()
      val endV = edge.end
      if(!endV.visited) {
        endV.visited = true
        depths.put(endV, depths(edge.start) + 1)
      }
      if(endV.key == end.key) return depths(endV)

      val l = endV.edges
        .filterNot(_.end.visited)

      leftEdges.enqueue(l:_*)
    }
    0
  }

  def conectivity(graph: Graph[Int]): List[List[Int]] = {
    def innerBFS(node: Vertex[Int]): List[Int] = {
      if(!node.visited){
        node.visited = true
        node.key :: node.edges.map(_.end).flatMap(innerBFS).toList
      } else List()
    }

    graph.vertices.map(v => innerBFS(v)).toList.filterNot(_.isEmpty)
  }
}
