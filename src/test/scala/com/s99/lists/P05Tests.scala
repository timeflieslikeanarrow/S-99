package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Find the number of elements of a list
def reverse[T](list: List[T]): List[T] = list match {
  case Nil => Nil
  case x::xs => reverse(xs) ++ List(x)
}

class P05Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === reverse(List()))
  }

  test("a single element list") {
    assert(List(1) == reverse(List(1)))
  }
  test("list with more than 1 elements") {
    assert(List(8, 5,3,2, 1, 1) === reverse(List(1,1,2,3,5,8)))
  }
