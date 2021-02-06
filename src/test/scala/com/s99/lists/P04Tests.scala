package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Find the number of elements of a list
def length[T](list: List[T]): Int = list match {
  case Nil => 0
  case _::xs => 1 + length(xs)
}

class P04Tests extends AnyFunSuite:
  test("empty list") {
    assert(0 === length(List()))
  }
  
  test("a single element list") {
    assert(1 == length(List(1)))
  }
  test("list with more than 1 elements") {
    assert(6 === length(List(1,1,2,3,5,8)))
  }
