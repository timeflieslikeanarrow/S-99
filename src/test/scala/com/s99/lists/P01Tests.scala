package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Find the last element of a list
def last[T](list: List[T]): T = list match {
  case Nil => throw NoSuchElementException("last of empty list")
  case x::Nil => x
  case _::xs => last(xs)
}

class P01Tests extends AnyFunSuite:
  test("empty list") {
    assertThrows[NoSuchElementException] {
      last(List())
    }
  }

  test("non-empty list") {
    assert(8 === last(List(1,1,2,3,5,8)))
  }
