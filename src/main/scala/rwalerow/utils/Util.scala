package rwalerow.utils

/**
  * Created by robert on 11.12.16.
  */
object Util {
  def min[E](ele: E*)(implicit comparator: (E, E) => Int): E = {
    implicit object o extends Ordering[E]{
      override def compare(x: E, y: E): Int = comparator(x, y)
    }
    ele.sorted.head
  }
}
