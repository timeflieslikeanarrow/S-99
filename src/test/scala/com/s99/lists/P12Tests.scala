package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Decode a running-length encoded list
def decode[T](list: List[(Int, T)]): List[T] =
  list.foldRight(List[T]()) { case ((n, e), r) => List.fill(n){e} ++ r }

class P12Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === decode(List()))
  }

  test("a single element list") {
    assert(List(1) === decode(List((1,1))))
  }

  test("a list of same elements") {
    assert(List(1, 1, 1) === decode(List((3, 1))))
  }

  test("list with more than 1 element") {
    assert(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')
      === decode(List(
      (4, 'a'),
      (1, 'b'),
      (2, 'c'),
      (2, 'a'),
      (1, 'd'),
      (4, 'e'))))
  }