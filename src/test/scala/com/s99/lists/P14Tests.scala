package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

//duplicate the elements of a list
def duplicate[T](list: List[T]): List[T] = list match
  case Nil => Nil
  case x::xs => x::x::duplicate(xs)
  
class P14Tests extends AnyFunSuite:
  test("empty list") {
    assert(List() === duplicate(List()))
  }

  test("a single element list") {
    assert(List(1,1) === duplicate(List(1)))
  }
  
  test("list with more than 1 element") {
   assert(List('a', 'a', 'b', 'b', 'c', 'c', 'c', 'c', 'd', 'd') == duplicate(List('a', 'b', 'c', 'c', 'd'))) 
  }