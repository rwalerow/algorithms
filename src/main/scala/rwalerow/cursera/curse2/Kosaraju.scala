package rwalerow.cursera.curse2

import rwalerow.cursera.curse2.Graph.{Graph, Vertex}

import scala.collection.mutable

object Kosaraju {

  def findStronglyConnected[K](graph: Graph[K]): List[Set[Vertex[K]]] = {

    val visited = mutable.Set[K]()
    val stack = mutable.Stack[K]()

    graph.vertices.foreach( vertex => {
      if(!visited.contains(vertex.key)) DFSFirst(vertex, visited, stack)
    })

    val reversedGraph = Graph.createReversedGraph(graph)
    visited.clear()
    var result = List[Set[Vertex[K]]]()

    stack.foreach(v => {
      val reversedVertex = reversedGraph.vertices.find(_.key == v).get
      if(!visited.contains(v)) result = DFSCollect(reversedVertex, visited) :: result
    })
   result
  }

  def DFSFirst[K](vertex: Vertex[K], visited: mutable.Set[K], stack: mutable.Stack[K]): Unit = {
    vertex.edges.map(_.end).foreach( v => {
      if(!visited.contains(v.key)) {
        visited.add(v.key)
        DFSFirst(v, visited, stack)
      }
    })
    stack.push(vertex.key)
  }

  def DFSCollect[K](vertex: Vertex[K], visited: mutable.Set[K]): Set[Vertex[K]] = {
    if(!visited.contains(vertex.key)){
      visited.add(vertex.key)
      vertex.edges.map(_.end).flatMap(end => DFSCollect(end, visited)).toSet + vertex
    } else Set()
  }
}
