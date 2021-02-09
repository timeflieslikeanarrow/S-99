package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Run-length encoding of a list (direct solution)
def encodeDirect[T](list: List[T]): List[(Int, T)] = 
  def iter(list: List[T], current: (Int, T), acc: List[(Int, T)]): List[(Int, T)] = list match {
    case Nil => (current :: acc).reverse
    case x::xs => if x != current._2
                  then iter(xs, (1, x), current::acc)
                  else iter(xs, (current._1 + 1, current._2), acc)
  }

  list match
    case Nil => Nil
    case x::xs => iter(xs, (1, x), List())

class P13Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === encodeDirect(List()))
  }

  test("a single element list") {
    assert(List((1, 1)) === encodeDirect(List(1)))
  }

  test("a list of same elements") {
    assert(List((3, 1)) === encodeDirect(List(1, 1, 1)))
  }

  test("list with more than 1 element") {
    assert(List(
      (4, 'a'),
      (1, 'b'),
      (2, 'c'),
      (2, 'a'),
      (1, 'd'),
      (4, 'e')) === encodeDirect(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e')))
  }