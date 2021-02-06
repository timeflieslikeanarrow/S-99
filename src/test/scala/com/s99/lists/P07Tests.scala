package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Flatten a nested list structure
def flatten[Any](list: List[Any]): List[Any] = list match {
  case Nil => Nil
  case (x:List[Any])::xs => flatten(x) ++ flatten(xs)
  case x::xs => x :: flatten(xs)
}

class P07Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === flatten(List()))
  }
  
  test("a single element list") {
    assert(List(1) === flatten(List(1)))
  }

  test("a single nested element list") {
    assert(List(1) === flatten(List(List(1))))
  }
  
  test("list with more than 1 element") {
    assert(List(1,1,2,3,5,8) === flatten(List(List(1,1), 2, List(3, List(5, 8)))))
  }