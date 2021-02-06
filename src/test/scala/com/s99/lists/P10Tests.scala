package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Run-length encoding of a list
def encode[T](list: List[T]): List[(Int, T)] =
  pack(list).map( xs => (xs.length, xs.head))

class P10Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === encode(List()))
  }

  test("a single element list") {
    assert(List((1, 1)) === encode(List(1)))
  }

  test("a list of same elements") {
    assert(List((3, 1)) === encode(List(1, 1, 1)))
  }

  test("list with more than 1 element") {
    assert(List(
      (4, 'a'),
      (1, 'b'),
      (2, 'c'),
      (2, 'a'),
      (1, 'd'),
      (4, 'e')) === encode(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
  }