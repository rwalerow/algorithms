package rwalerow.cormen

/**
  * Created by robert on 30.10.16.
  */
object MergeSortInPlace {
  def mergeSortInPlace(a: Array[Int]): Array[Int] = {
    val result = a.clone()

    def mergeSortInner(arr: Array[Int], p: Int, r: Int): Unit = {
      if(p < r) {
        val q = (p + r)/2
        mergeSortInner(arr, p, q)
        mergeSortInner(arr, q + 1, r)
        merge(arr, p, q, r)
      }
    }

    def merge(arr: Array[Int], p: Int, q: Int, r: Int): Unit = {
      println(s"merge((${arr.mkString(",")}), p = ${p}, q = ${q}, r = ${r})")
      var first = p + 1 // 1
      var second = q + 1 // 2
      var help = arr(p) // 2

      if(r - p == 1){
        if(help > arr(r)){
          arr(p) = arr(r)
          arr(r) = help
        }
      } else {

        (p to r).foreach(i => { // 0
          if(first > q || second > r) {
            if(first >= q && second >= r){
              arr(i) = help
            } else if(first >= q) {
              placeSecond(i)
            } else {
              placeFirst(i)
            }
          } else if(arr(first) < arr(second)) {
            placeFirst(i)
          } else {
            placeSecond(i)
          }
        })}
      println(s"arr: ${arr.mkString(",")}")
      def placeFirst(i: Int): Unit = {
        if(help < arr(first)){
          arr(i) = help
          help = arr(first)
        } else {
          arr(i) = arr(first)
        }
        first += 1
      }

      def placeSecond(i: Int): Unit = {
        if(help < arr(second)){
          arr(i) = help
          help = arr(second)
        } else {
          arr(i) = arr(second)
        }
        second += 1
      }
    }

    mergeSortInner(result, 0, result.length - 1)
    result
  }
}
