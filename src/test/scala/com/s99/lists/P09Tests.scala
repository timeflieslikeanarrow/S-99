package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Pack consecutive duplicates of list elements into sublists
def pack[T](list: List[T]): List[List[T]] = list match {
  case Nil => Nil
  case x :: _ => list.takeWhile(_ == x) :: pack(list dropWhile(_ == x))
}

class P09Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === pack(List()))
  }

  test("a single element list") {
    assert(List(List(1)) === pack(List(1)))
  }

  test("a list of same elements") {
    assert(List(List(1, 1, 1)) === pack(List(1,1,1)))
  }

  test("list with more than 1 element") {
    assert(List(
      List('a', 'a', 'a', 'a'), 
      List('b'),
      List('c', 'c'), 
      List('a', 'a'),
      List('d'),
      List('e', 'e', 'e', 'e')) === pack(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
  }