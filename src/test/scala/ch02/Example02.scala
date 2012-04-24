package ch02

import org.specs2.mutable._

class TurnsSpec extends Specification {
  "turns" should {
    "10 find 1 try range 0 to 10" in {
      Tuns.find(5, 0, 10) == 1
    }
  }

}

class AddSpec extends Specification {
  "add" should {
    "1 2 + 1 3 5 = 1 4 7" in {
      Add.execute(List(0, 1, 2), List(1, 3, 5)) == List(1, 4, 7)
    }

    "carry at 1 digit 8 + 4 = 1 2" in {
      Add.execute(List(0, 8), List(0, 4))  == List(1, 2)
    }
  }
}
