package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Split a list into two parts
def split[T](n: Int, list: List[T]): (List[T], List[T]) =
  (list.take(n), list.drop(n))

class P17Tests extends AnyFunSuite:
  test("empty list") {
    assert((List(), List()) === split(3, List()))
  }

  test("a single element list") {
    assert((List(1), List()) === split(3, List(1)))
  }

  test("list with more than 1 element") {
    assert((List('a', 'b', 'c'), List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
      == split(3, List('a', 'b', 'c',  'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }