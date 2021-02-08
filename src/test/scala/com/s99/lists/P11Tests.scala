package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Modified run-length encoding
def encodeModified[T](list: List[T]): List[Any] =
  pack(list).map( xs => if xs.length == 1 then xs.head else (xs.length, xs.head))

class P11Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === encodeModified(List()))
  }

  test("a single element list") {
    assert(List(1) === encodeModified(List(1)))
  }

  test("a list of same elements") {
    assert(List((3, 1)) === encodeModified(List(1, 1, 1)))
  }

  test("list with more than 1 element") {
    assert(List(
      (4, 'a'),
      'b',
      (2, 'c'),
      (2, 'a'),
      'd',
      (4, 'e')) === encodeModified(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
  }