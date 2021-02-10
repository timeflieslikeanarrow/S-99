package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Remove the kth element from a list
def removeAt[T](k: Int, list: List[T]): (List[T], T) =
  if k < 0 || k >= list.length then
    throw NoSuchElementException(s"invalid index $k for removeAt")
  else
    val (front, back) = list.splitAt(k)
    (front ++ back.tail, back.head)

class P20Tests extends AnyFunSuite:
  test("empty list") {
    assertThrows[NoSuchElementException] {
      removeAt(1, List())
    } 
  }
  test("a single element list") {
    assert((List(), 1) === removeAt(0, List(1)))
  }

  test("list with more than 1 element") {
    assert((List('a', 'c', 'd'), 'b')
      == removeAt(1, List('a', 'b', 'c', 'd')))
  }