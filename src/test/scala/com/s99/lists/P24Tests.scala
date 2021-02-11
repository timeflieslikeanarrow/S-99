package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Lotto: Draw N different random numbers from the set 1..M.
def lotto[T](N: Int,  M: Int): List[Int] =
  if N > M then throw NoSuchElementException("not enough elements")
  else
    val random = scala.util.Random(42)
    val list = (1 to M).toList
    random.shuffle(list).take(N)
    
class P24Tests extends AnyFunSuite:
  test("empty list") {
    assertThrows[NoSuchElementException] {
      randomSelect(1, List())
    }
  }

  test("a single element list") {
    assert(List(1) === lotto(1, 1))
  }

  test("list with more than 1 element") {
    assert(List(4,34,8,24,26,40)
      == lotto(6, 49))
  }