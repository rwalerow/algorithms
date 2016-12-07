package rwalerow.utils

/**
  * Created by robert on 02.12.16.
  */

trait CompareResult
object Smaller extends CompareResult
object Equal extends CompareResult
object Grater extends CompareResult

trait Compare[T] {
  def compare(first: T, second: T): CompareResult
  def grater(first: T, second:T): Boolean = compare(first, second) == Grater
  def equal(first: T, second:T): Boolean = compare(first, second) == Equal
  def smaller(first: T, second:T): Boolean = compare(first, second) == Smaller

}
object Compare {
  def apply[A: Compare]: Compare[A] = implicitly
}

object CompareImplicits {
  implicit object CompareInts extends Compare[Int] {
    override def compare(first: Int, second: Int): CompareResult =
      if(first < second) Smaller
      else if(first == second) Equal
      else Grater
  }
}
