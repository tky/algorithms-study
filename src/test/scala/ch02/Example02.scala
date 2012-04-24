package ch02

import org.specs2.mutable._

class TurnsSpec extends Specification {
  "turns" should {
    "10 find 1 try range 0 to 10" in {
      Tuns.find(5, 0, 10) == 1
    }
  }

}
