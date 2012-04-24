package ch02

/**
 * Chapter 2
 *  ex2-1
*/
object Tuns extends App {

  def find(target: Int, low: Int, high: Int): Int = {
    dump(low,high)
    if ((high - low) >= 2) {
      ((low + high) / 2) match {
        case mid if mid == target => 1
        case mid if mid < target => 1 + find(target, mid + 1, high)
        case mid if mid >= target => 1 + find(target, low, mid - 1)
      }
    } else {
     1
    }
  }

  def dump(low: Int, high: Int): Unit = {
    println("low: %s hight: %s".format(low, high))
  }
}

/**
 * 2-2 加算の実験
 *
 * ２つのリストに入っている数字を加算する。
 * ２つのリストは同じサイズでなければならない。
 * もし、異なる桁の数字を加算する場合は０で埋めなければならない。
 * ex)
 * 12 + 135 = 147
 * Add.execute(List(0, 1, 2), List(1, 3, 5) = List(1, 4, 7)
*/
object Add extends App {
  def execute (n1: List[Int], n2: List[Int]): List[Int] = {
    ((n1 zip n2).foldRight((List.empty[Int], 0)) {
        (elem, sum) => Util.calc(elem._1, elem._2) match {case (value, carry) => ((value + sum._2) :: sum._1, carry)}
    })._1
  }


  /**
   * 桁違いのリストに対応版
     最初に桁をあわせる処理を行い、最後にcarryが残っていれば先頭に追加。　
  */
  def execute2 (n1: List[Int], n2: List[Int]): List[Int] = {
    (((n1, n2) match {
      case (n1, n2) if n1.length > n2.length => n1 zip (Util.zeroList(n1.length - n2.length) ++ n2)
      case (n1, n2) if n1.length < n2.length => (Util.zeroList(n2.length - n1.length) ++ n1) zip  n2 
      case (n1, n2) => n1 zip n2
    }).foldRight((List.empty[Int], 0)) {
        (elem, sum) => Util.calc(elem._1, elem._2) match {case (value, carry) => ((value + sum._2) :: sum._1, carry)}
    }) match {
      case (xs, carry) if carry > 0 => 1 :: xs
      case (xs, _) => xs
    }
  }

  /*
   * Nil対応版。
   * mut1を作るために必要だったけど、、
  */
  def execute3 (n1: List[Int], n2: List[Int]): List[Int] = (n1, n2) match {
    case (n1, Nil) => n1
    case (Nil, n2) => n2
    case (n1, n2) => execute2(n1, n2)
  }
}

object Mult extends App {
  private def mult(xs: List[Int], x: Int): List[Int] = {
    (xs.foldRight(List.empty[Int], 0){ (elem, sum) =>
      val value = elem * x + sum._2
      if (value > 9) {
        ((value % 10) :: sum._1, value / 10)
      } else {
        (value :: sum._1, 0)
      }
    }) match {
      case (xs, carry) if carry > 0 => carry :: xs
      case (xs, _) => xs
    }
  }

  def execute (n1: List[Int], n2: List[Int]): List[Int] = {
    (n2.foldRight((List.empty[Int], 0)) {
      (elem, sum) => {
       (Add.execute3(mult(n1, elem) ++ Util.zeroList(sum._2), sum._1), sum._2 + 1)
      }
    })._1
  }
}

object Util {
  def calc(x: Int, y: Int): (Int, Int) =
    if (x + y > 9) {
      ((x + y) %  10, 1)
    } else {(x + y, 0)}

  def zeroList(n: Int): List[Int] = if (n > 0) { 0 :: zeroList(n - 1)} else { Nil }
}
