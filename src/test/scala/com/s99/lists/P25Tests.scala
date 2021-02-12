package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Generate a random permutation of the elements of a list.
def randomPermute[T](list: List[T]): List[T] =
  randomSelect(list.length, list)

class P25Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === randomPermute(List()))
  }

  test("a single element list") {
    assert(List(1) === randomPermute(List(1)))
  }

  test("list with more than 1 element") {
    assert(List('d', 'g', 'c', 'a', 'f', 'e', 'b')
      == randomPermute(List('a', 'b', 'c', 'd', 'e', 'f', 'g')))
  }