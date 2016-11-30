package rwalerow.mit6006.graphs

import rwalerow.mit6006.graphs.Graph.Graph

import scala.collection.mutable

object DFS {

  def detectCycle[K](graph: Graph[K]): Boolean = {

    val parents = mutable.HashMap[K, Option[K]]()
    val processed = mutable.HashMap.empty[K, Boolean].withDefaultValue(false)

    def dfsVisit(vertex: K): Boolean = {
      if(parents.keySet.contains(vertex)) return false
      else {
        processed(vertex) = true

        for(s <- graph(vertex)) {
          if(processed(s)) return true
          else {
            dfsVisit(s)
            parents(s) = Some(vertex)
          }
        }

        processed(vertex) = false
      }
      false
    }

    graph.keys.exists(dfsVisit)
  }

  def topologicalOrder[K](graph: Graph[K]): List[K] = {

    val parents = mutable.HashMap[K, Option[K]]()
    val stack = mutable.Stack[K]()

    def dfsVisit(vertex: K): Unit = {
      if(!parents.keySet.contains(vertex)){
        for(s <- graph(vertex)){
          dfsVisit(s)
          parents(s) = Some(vertex)
        }
        stack.push(vertex)
        parents(vertex) = None
      }
    }
    graph.keys.foreach(dfsVisit)
    stack.toList
  }
}
