package ch02

/**
 * Chapter 2
 *  ex2-1
*/
object Example02 extends App {

  def tuns(target: Int, low: Int, high: Int): Int = {
    dump(low,high)
    if ((high - low) >= 2) {
      ((low + high) / 2) match {
        case mid if mid == target => 1
        case mid if mid < target => 1 + tuns(target, mid + 1, high)
        case mid if mid >= target => 1 + tuns(target, low, mid - 1)
      }
    } else {
     1
    }
  }

  def dump(low: Int, high: Int): Unit = {
    println("low: %s hight: %s".format(low, high))
  }
}
