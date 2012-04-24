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
    def calc(x: Int, y: Int): (Int, Int) =
      if (x + y > 9) {
        ((x + y) %  10, 1)
      } else {(x + y, 0)}

    ((n1 zip n2).foldRight((List.empty[Int], 0)) {
        (elem, sum) => calc(elem._1, elem._2) match {case (value, carry) => ((value + sum._2) :: sum._1, carry)}
    })._1
  }

}
