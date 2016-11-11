package rwalerow.cursera.curse1

//  object CompareUtils{
//
//    sealed trait Eq
//    object GT extends Eq
//    object EQ extends Eq
//    object Lt extends Eq
//
//    trait Compare[A] {
//      def compare(first: A, Second: A): Eq
//    }
//
//    implicit val intCompare = new Compare[Int]{
//      override def compare(first: Int, second: Int): Eq = first.compare(second)
//    }
//  }

object Mergesort {

  def sort(toSort: List[Int]): List[Int] = toSort match {
    case Nil => Nil
    case l@(head :: Nil) => l
    case l => merge(sort(l.take(toSort.size/2)), sort(l.drop(toSort.size/2)))
  }

  def merge(first: List[Int], second: List[Int]): List[Int] = (first, second) match {
    case (Nil, Nil) => Nil
    case (f, Nil) => f
    case (Nil, s) => s
    case (f::ft, s::st) => if(f < s) f :: merge(ft, second) else s :: merge(first, st)
  }

  def sort(toSort: Array[Int]): Array[Int] = {

    def merge(first: Array[Int], second: Array[Int]): Array[Int] = {
      if (first.isEmpty) second
      else if (second.isEmpty) first
      else {

        var i = 0
        var j = 0
        val result = Array.fill(toSort.length) {0}

        toSort.indices.foreach(k =>
          if(i == first.length) {
            result(k) = second(j)
            j += 1
          } else if(j == second.length) {
            result(k) = first(i)
            i += 1
          } else if (first(i) < second(j)) {
            result(k) = first(i)
            i += 1
          } else {
            result(k) = second(j)
            j += 1
          }
        )
        result
      }
    }
    if (toSort.size == 1) toSort
    else {
      val splitPoint = toSort.size / 2
      merge(sort(toSort.take(splitPoint)), sort(toSort.drop(splitPoint)))
    }
  }
}
