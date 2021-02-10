package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Insert an element at a given position into a list
def insertAt[T](item: T, k: Int, list: List[T]): List[T] =
  if k < 0 || k >= list.length then
    throw NoSuchElementException(s"invalid index $k for removeAt")
  else
    list.take(k) ++ (item::list.drop(k))
    

class P21Tests extends AnyFunSuite:
  test("empty list") {
    assertThrows[NoSuchElementException] {
      insertAt(1, 1, List())
    }
  }
  test("a single element list") {
    assert(List(10, 1) === insertAt(10, 0, List(1)))
  }

  test("list with more than 1 element") {
    assert(List('a', 'n', 'b', 'c', 'd')
      == insertAt('n', 1, List('a', 'b', 'c', 'd')))
  }