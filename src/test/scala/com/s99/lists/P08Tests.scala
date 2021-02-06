package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Eliminate consecutive duplicates of list elements
def compress[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case x :: xs => x::compress(xs dropWhile(_ == x))
  }

class P08Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === compress(List()))
  }

  test("a single element list") {
    assert(List(1) === compress(List(1)))
  }

  test("a list of same elements") {
    assert(List(1) === compress(List(1,1,1)))
  }

  test("list with more than 1 element") {
    assert(List('a', 'b', 'c', 'a', 'd', 'e') === compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
  }