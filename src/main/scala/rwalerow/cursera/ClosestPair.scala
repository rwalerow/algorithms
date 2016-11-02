package rwalerow.cursera
import Math._
/**
  * Created by robert on 02.11.16.
  */
object ClosestPair {

  case class Point(x: Int, y: Int)

  def find(points: Array[Point]): (Point, Point) = {
    val px = points.sortBy(_.x)
    val py = points.sortBy(_.y)

    closestPair(px, py)
  }

  def distance(first: Point, second: Point): Double =
    sqrt(pow(first.x - second.x, 2.0) + pow(first.y - second.y, 2.0))

  private def findSmallLength(px: Array[Point]): (Point, Point) = {
    var best = Double.MaxValue
    var bestPair = (Point(0, 0), Point(0, 0))

    if(px.length == 2){
      return (px(0), px(1))
    } else {
      (0 until px.length - 1).foreach(i => {
        (i + 1 until px.length).foreach(j => {
          if (distance(px(i), px(j)) < best) {
            best = distance(px(i), px(j))
            bestPair = (px(i), px(j))
          }
        })
      })

      bestPair
    }
  }

  def closestPair(px: Array[Point], py: Array[Point]): (Point, Point) = {
    if(px.length <= 5){
      findSmallLength(px)
    } else {
      val qx = px.take(px.length / 2)
      val qy = py.take(py.length / 2)

      val rx = px.drop(px.length / 2)
      val ry = py.drop(py.length / 2)

      val a1@(p1, q1) = closestPair(qx, qy)
      val a2@(p2, q2) = closestPair(rx, ry)
      val minDist = min(distance(p1, q1), distance(p2, q2))
      val a3@(p3, q3) = closestSplitPair(px, py, minDist)

      var best = a1
      if(distance(p2, q2) < distance(p1, q1)){
        best = a2
      }
      if(distance(p3, q3) < distance(best._1, best._2)){
        best = a3
      }
      best
    }
  }

  def closestSplitPair(px: Array[Point], py: Array[Point], delta: Double): (Point, Point) = {
    val xbar = px(px.length/2).x
    val sy = py.filter(p => xbar - delta <= p.x && p.x <= xbar + delta)

    var best = delta
    var bestPair: (Point, Point) = (px(0), px(px.length-1))

    (0 until sy.length - 7).foreach( i => {
      (0 until 7).foreach( j => {
        val (p1, p2) = (sy(i), sy(i + j))
        if(distance(p1, p2) < best){
          bestPair = (p1, p2)
          best = distance(p1, p2)
        }
      })
    })

    bestPair
  }
}
