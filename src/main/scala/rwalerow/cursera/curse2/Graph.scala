package rwalerow.cursera.curse2

import scala.collection.mutable.ListBuffer

object Graph {
  case class Graph[K](vertices: ListBuffer[Vertex[K]])
  case class Edge[K](start: Vertex[K], end: Vertex[K])
  class Vertex[K](var key: K, var edges: ListBuffer[Edge[K]], var visited: Boolean = false, var depth: Int = 0)

  def extendVertices[K, E <: Vertex[K]](inputGraph: Graph[K], extendFunction: Vertex[K] => E): Graph[K] = {
    Graph(inputGraph.vertices.map(extendFunction))
  }

  def createSpecialGraph[K](verticesList: List[K], edges: List[(K, K)], createVertex: (K, ListBuffer[Edge[K]]) => Vertex[K]): Graph[K] = {
    val allVertices = verticesList.to[ListBuffer].map(e => createVertex(e, ListBuffer[Edge[K]]()))
    for(edge <- edges) {
      val startVertex = allVertices.find(_.key equals edge._1)
      val endVertex = allVertices.find(_.key equals edge._2)

      for {
        start <- startVertex
        end <- endVertex
      } start.edges.append(Edge(start, end))
    }
    Graph(allVertices)
  }

//  def createGraph(verticesCount: Int, edges: List[(Int, Int)]): Graph[Int, Vertex[Int]] = {
//    val allVertices: ListBuffer[Vertex[Int]] = ListBuffer.range(1, verticesCount).map(new Vertex(_, ListBuffer[Edge[Int]]()))
//    for(edge <- edges) {
//      val startVertex = allVertices.find(_.key == edge._1)
//      val endVertex = allVertices.find(_.key == edge._2)
//
//      for{
//        start <- startVertex
//        end <- endVertex
//      } start.edges.append(Edge(start, end))
//    }
//    Graph(allVertices)
//  }
}