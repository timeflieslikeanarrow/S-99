package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//duplicate the elements of a list a give number of times
def drop[T](n: Int, list: List[T]): List[T] =
  if list.isEmpty then list
  else list.take(n-1) ++ drop(n, list.drop(n))

class P16Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === drop(3, List()))
  }

  test("a single element list") {
    assert(List() === drop(1, List(1)))
  }

  test("list with more than 1 element") {
    assert(List('a', 'b',  'd', 'e', 'g', 'h', 'j', 'k')
      == drop(3, List('a', 'b', 'c',  'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }