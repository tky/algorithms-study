package ch04

import org.specs2.mutable._

class InsertSortSpec extends Specification {
  "insert" should {
    "1 element list" in {
      InsertSort.execute(List(1)){_ < _} == List(1)
    }

    " 2 elements list" in {
      InsertSort.execute(List(2, 1)){_ < _}  == List(1, 2)
    }

    "long lements list " in {
      InsertSort.execute(List(3, 6, 1, 2, 4, 5)){_ < _} == List(1, 2, 3, 4, 5, 6)
    }
  }
}
