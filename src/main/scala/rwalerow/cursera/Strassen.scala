package rwalerow.cursera

/**
  * Created by robert on 01.11.16.
  */
object Strassen {

  type MatrixDim = Array[Array[Int]]

  abstract class MatrixExtensions(m: MatrixDim) {

  }

//  implicit def matrixExtension(m: MatrixDim) = new MatrixExtensions() {
//    def +(m2: MatrixDim) =
//  }

  case class Matrix(matrix: MatrixDim, width: Int, heigh: Int) {
    def +(other: Matrix): Matrix = {
      Matrix(plus(matrix, other.matrix), width, heigh)
    }
  }

  private def plus(first: MatrixDim, second: MatrixDim): MatrixDim = {
    val width = first.length
    val height = first(0).length
    val arr = Array.ofDim[Int](width, height)

    for {
      x <- 0 until width
      y <- 0 until height
    } arr(x)(y) = first(x)(y) + second(x)(y)

    arr
  }

  private def subtract(first: MatrixDim, second: MatrixDim): MatrixDim = {
    val width = first.length
    val height = first(0).length
    val arr = Array.ofDim[Int](width, height)

    for {
      x <- 0 until width
      y <- 0 until height
    } arr(x)(y) = first(x)(y) - second(x)(y)

    arr
  }

  /**
    * Postponed, becauce of missing explination of odd cases
    * Get back to it with cormen
    */
  //  def multiply(first: Matrix, second: Matrix): Matrix = (first, second) match {
//    case (Matrix(m1, x1, y1), Matrix(m2, x2, y2)) if x1 == x2 == y1 == y2 == 1 => {
//      Matrix(Array.fill(1,1){m1(0)(0) * m2(0)(0)}, x1, y1)
//    }
//    case (Matrix(m1, x1, y1), Matrix(m2, x2, y2)) if x1 == y2 == 1 && y1 == x2 == 2 => {
//      val result = m1(0)(0) * m2(0)(0) + m1(1)(0) * m2(0)(1)
//      Matrix(Array.fill(1,1){result}, 1, 1)
//    }
//    case (Matrix(m1, x1, y1), Matrix(m2, x2, y2)) if x1 == y2 == 2 && y1 == x2 == 1 => {
//      val result = Array.fill(2,2){0}
//      result(0)(0) = m1(0)(0) * m2(0)(0)
//      result(0)(1) = m1(0)(1) * m2(0)(0)
//      result(1)(0) = m1(0)(0) * m2(1)(0)
//      result(1)(1) = m1(0)(1) * m2(1)(0)
//      Matrix(result, 2, 2)
//    }
//    case (Matrix(m1, x1, y1), Matrix(m2, x2, y2)) if y1 == 1 && x1 == y1 == x2 == 2 => {
//      val result = Array.fill(2, 1){0}
//      result(0)(0) = m1(0)(0) * m2(0)(0) + m1(1)(0) * m2(0)(1)
//      result(1)(0) = m1(0)(0) * m2(1)(0) + m1(1)(0) * m2(1)(1)
//      Matrix(result, 2, 1)
//    }
//    case (Matrix(m1, x1, y1), Matrix(m2, x2, y2)) if x1 == y1 == y2 == 2 && x2 == 1 => {
//      val result = Array.fill(1,2){0}
//      result(0)(0) = m1(0)(0) * m2(0)(0) + m1(1)(0) * m2(0)(1)
//      result(0)(1) = m1(0)(1) * m2(0)(0) + m1(1)(1) * m2(0)(1)
//      Matrix(result, 1, 2)
//    }
//    case (Matrix(m1, x1, y1), Matrix(m2, x2, y2)) if x1 == y1 == x2 == y2 == 2 => {
//      val result = Array.fill(2,2){0}
//      result(0)(0) = m1(0)(0) * m2(0)(0) + m1(1)(0) * m2(0)(1)
//      result(0)(1) = m1(0)(1) * m2(0)(0) + m1(1)(1) * m2(0)(1)
//      result(1)(0) = m1(0)(0) * m2(1)(0) + m1(1)(0) * m2(1)(1)
//      result(1)(1) = m1(0)(1) * m2(1)(0) + m1(1)(1) * m2(1)(1)
//      Matrix(result, 2, 2)
//    }
//    case (Matrix(m1, x1, y1), Matrix(m2, x2, y2)) => {
//      val a = m1.take(x1/2).map(_.take(y1/2))
//      val b = m1.drop(x1/2).map(_.take(y1/2))
//      val c = m1.take(x1/2).map(_.drop(y1/2))
//      val d = m1.drop(x1/2).map(_.drop(y1/2))
//
//      val e = m2.take(x2/2).map(_.take(y2/2))
//      val f = m2.drop(x2/2).map(_.take(y2/2))
//      val g = m2.take(x2/2).map(_.drop(y2/2))
//      val h = m2.drop(x2/2).map(_.drop(y2/2))
//
//      val p1 = ???
//      val p2 = ???
//      val p3 = ???
//      val p4 = ???
//      val p5 = ???
//      val p6 = ???
//      val p7 = ???
//    }
//  }

}
