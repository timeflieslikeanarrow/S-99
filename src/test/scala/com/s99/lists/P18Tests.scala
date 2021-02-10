package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Extract a slice from a list
def slice[T](start: Int, end: Int, list: List[T]): List[T] =
  list.drop(start).take(end - start)

class P18Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === slice(3, 7, List()))
  }

  test("a single element list") {
    assert(List() === slice(3, 7, List(1)))
  }

  test("list with more than 1 element") {
    assert(List('d', 'e', 'f', 'g')
      == slice(3, 7, List('a', 'b', 'c',  'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }