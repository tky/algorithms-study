package ch04

object InsertSort {

  def execute[A](xs: List[A])(f: (A, A) => Boolean): List[A] = {
    xs.foldLeft(List.empty[A]) { (sum, elem) =>
      if (sum.isEmpty) {
        elem :: Nil
      } else {
        val (head, tail) = sum.span{ f(_, elem) }
        head ::: elem :: tail
      }
    }
  }
}
