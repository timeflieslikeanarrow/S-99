package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Extract a given number of randomly selected elements from a list.
//Hint: Use the solution to problem P20
def randomSelect[T](n: Int, list: List[T]): List[T] =
  if n > list.length then throw NoSuchElementException("not enough elements")
  else
    val r = scala.util.Random(42)
    (1 to n).foldRight((list, List[T]())) { 
      case (_, (ls, acc)) => 
        val (rest, item) = removeAt(r.nextInt(ls.length), ls)
        (rest, item :: acc)
    }._2

class P23Tests extends AnyFunSuite:
  test("empty list") {
    assertThrows[NoSuchElementException] {
      randomSelect(1, List())
    }
  }
  
  test("a single element list") {
    
    assert(List(1) === randomSelect(1, List(1)))
  }

  test("list with more than 1 element") {
    assert(List('f', 'e', 'b')
      == randomSelect(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g')))
  }