package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//duplicate the elements of a list a give number of times
def duplicateN[T](n: Int, list: List[T]): List[T] = list match
  case Nil => Nil
  case x::xs => List.fill(n)(x) ++ duplicateN(n, xs)

class P15Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === duplicateN(3, List()))
  }

  test("a single element list") {
    assert(List(1,1, 1) === duplicateN(3, List(1)))
  }

  test("list with more than 1 element") {
    assert(List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd') 
      == duplicateN(3, List('a', 'b', 'c', 'c', 'd')))
  }