package rwalerow

/**
  * Created by robert on 30.10.16.
  */
object Run extends App {

  import rwalerow.cormen.MergeSortInPlace._
  val a = Array(2,4,3,1)

  mergeSortInPlace(a).mkString(",")

}
