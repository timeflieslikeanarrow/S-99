package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Generate the combinations of K distinct objects chosen from the N elements of a list.
def combinations[T](k: Int, list: List[T]): List[List[T]] = 
  val result = scala.collection.mutable.Set[Set[T]]()
  
  def iter(n: Int, current: Set[T], rest: List[T]): Unit = {
    if n == k then result += current
    else
      for i <- 0 until rest.length do 
        val (remaining, item) = removeAt(i, rest)
        iter(n + 1, current + item, remaining)
  }

  if k > list.length then throw NoSuchElementException("selection larger than list")
  else {
    iter(0, Set(), list)
    result.map(_.toList).toList
  }
  
class P26Tests extends AnyFunSuite:
  test("empty list") {
    assertThrows[NoSuchElementException] {
     combinations(1, List())
    }
  }

  test("a single element list") {
    assert(List(List(1)) === combinations(1, List(1)))
  }

  test("C(6,3) === 20") {
    val result = combinations(3, List('a', 'b', 'c', 'd', 'e', 'f'))
    println(result)
    assert(20 === result.size)
  }
  test("C(12,3) === 220") {
    val result = combinations(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k','l'))
    println(result)
    assert(220 === result.size)
  }