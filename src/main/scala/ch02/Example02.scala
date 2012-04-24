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
