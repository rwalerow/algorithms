package rwalerow.mit6006.graphs

import rwalerow.mit6006.graphs.Graph.Graph

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by robert on 29.11.16.
  */
object BFS {

  def fromTo[K](start: K, graph: Graph[K]): mutable.HashMap[K, Int] = {
    val levels = mutable.HashMap[K, Int]{ start -> 0 }
    val parents = mutable.HashMap[K, Option[K]]{ start -> None }

    var i = 1
    var frontier = List(start)

    while(frontier.nonEmpty) {
      var nextLevel = ListBuffer[K]()

      frontier.foreach( u =>
        graph(u).foreach( end =>
          if(!levels.keySet.contains(end)) {
            levels.put(end, i)
            parents.put(end, Some(u))
            nextLevel.append(end)
          }
        )
      )

      frontier = nextLevel.toList
      i += 1
    }
    levels
  }
}
