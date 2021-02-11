package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Create a list containing all integers within a given range
def range(start: Int, end: Int): List[Int] =
  //List.range(start, end + 1)
  if start > end then Nil
  else start :: range(start + 1, end)

class P22Tests extends AnyFunSuite:
  test("a single element list") {
    assert(List(1) === range(1,1))
  }

  test("list with more than 1 element") {
    assert(List(4,5,6,7,8,9)
      === range(4,9))
  }