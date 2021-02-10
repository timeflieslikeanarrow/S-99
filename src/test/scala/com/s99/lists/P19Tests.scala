package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//Rotate a list N places to the left
def rotate[T](n: Int, list: List[T]): List[T] = 
  if list.length < 2 then list
  else 
    val m = (list.length + n % list.length) % list.length
    list.drop(m) ++ list.take(m)

class P19Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === rotate(3, List()))
  }

  test("a single element list") {
    assert(List(1) === rotate(3, List(1)))
  }

  test("list with more than 1 element") {
    assert(List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c')
      == rotate(3, List('a', 'b', 'c',  'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }

  test("roate with negative n") {
    assert(List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
      == rotate(-2, List('a', 'b', 'c',  'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }

  test("roate with large n") {
    assert(List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c')
      == rotate(102, List('a', 'b', 'c',  'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }

  test("roate with large negative n") {
    assert(List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i')
      == rotate(-101, List('a', 'b', 'c',  'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
  }