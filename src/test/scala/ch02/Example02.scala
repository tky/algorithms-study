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

  "add2" should {
    "1 2 + 1 3 5 = 1 4 7" in {
      Add.execute2(List(1, 2), List(1, 3, 5)) == List(1, 4, 7)
    }

    "carry at 1 digit 8 + 4 = 1 2" in {
      Add.execute2(List(8), List(4))  == List(1, 2)
    }
  }
}

class MultSpec extends Specification {
  "mult" should {
    "same digit and not carried 2 * 3" in {
      Mult.execute(List(2), List(3)) == List(6)
    }

    "different digit and not carried 243 * 2" in {
      Mult.execute(List(2, 4, 3), List(2)) == List(4, 8, 6)
    }
    "right number is longer and ,different digit and not carried 243 * 2" in {
      Mult.execute(List(2), List(2, 4, 3)) == List(4, 8, 6)
    }
    "same digit and carried 6 * 3" in {
      Mult.execute(List(6), List(3)) == List(1, 8)
    }
    "different digit and carried 6 * 3" in {
      Mult.execute(List(1, 7, 4), List(3, 6)) == List(6, 2, 6, 4)
    }
  }
}
