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

object QuickSort {
  def execute[A](xs: List[A])(implicit f: (A, A) => Boolean): List[A] = xs match {
    case l @ (x :: Nil) => l
    case (x :: y :: Nil) => if(f(x, y)) { List(x, y) } else { List(y, x) }
    // クイックソートはどこを基準にとっても同じなので、先頭を基準値として採用。　
    case (x :: ls) => {
      val heads = ls.filter(f(_, x))
      val tails = ls.filterNot(f(_, x))
      heads ::: x :: execute(tails)
    }
    case _ => Nil
  }
}
