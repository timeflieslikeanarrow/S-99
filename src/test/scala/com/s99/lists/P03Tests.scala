package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Find the kth element of a list
def nth[T](k: Int, list: List[T]): T = list match {
  case Nil => throw NoSuchElementException("nth for empty list")
  case x::_ if k == 0 => x
  case _::xs => nth(k-1, xs)
}

class P03Tests extends AnyFunSuite:
  test("empty list") {
    assertThrows[NoSuchElementException] {
      nth(1, List())
    }
  }
  test("a single element list") {
    assertThrows[NoSuchElementException] {
      nth(1, List(1))
    }
  }
  test("list with more than 1 elements") {
    assert(2 === nth(2, List(1,1,2,3,5,8)))
  }
