package ch04

object InsertSort {

  // TODO:execute[A <: Ordered[A]
  def execute[A <: Int](xs: List[A]): List[A] = {
    xs.foldLeft(List.empty[A]) { (sum, elem) =>
      if (sum.isEmpty) {
        elem :: Nil
      } else {
        val (head, tail) = sum.span{_ < elem }
        head ::: elem :: tail
      }
    }
  }
}
