package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Find the last but one element of a list
def penultimate[T](list: List[T]): T = list match {
  case Nil | List(_) => throw NoSuchElementException("penultimate for empty or single element list")
  case List(x, _) => x
  case _::xs => penultimate(xs)
}
  
class P02Tests extends AnyFunSuite:
  test("empty list") {
    assertThrows[NoSuchElementException] {
      penultimate(List())
    }
  }
  test("a single element list") {
    assertThrows[NoSuchElementException] {
      penultimate(List(1))
    }
  }
  test("list with more than 1 element") {
    assert(5 === penultimate(List(1,1,2,3,5,8)))
  }
