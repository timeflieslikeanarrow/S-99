package com.s99.lists

import org.scalatest.funsuite.AnyFunSuite

import java.util.NoSuchElementException

//Sorting a list of lists according to length of sublists.
def lsort[T](list: List[List[T]]): List[List[T]] =
  list.sortWith(_.length < _.length)

// sort the elements according to their length frequency
def lsortFreq[T](list: List[List[T]]): List[List[T]] = {
  val frequencies = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)
  list.foreach ( ls =>
    frequencies(ls.length) += 1
  )
  
  list.sortWith { case (ls1, ls2) => frequencies(ls1.length) < frequencies(ls2.length) }
}

class P28Tests extends AnyFunSuite:
  test("lsort") {
    val result = lsort(List(List('a', 'b', 'c'), List('d', 'e'), List('f', 'g', 'h'), List('d', 'e'), List('i', 'j', 'k', 'l'), List('m', 'n'), List('o')))
    assert(List(List('o'), List('d', 'e'), List('d', 'e'), List('m', 'n'), List('a', 'b', 'c'), List('f', 'g', 'h'), List('i', 'j', 'k', 'l'))
     === result)
  }

  test("lsortFreq") {
    val result = lsortFreq(List(List('a', 'b', 'c'), List('d', 'e'), List('f', 'g', 'h'), List('d', 'e'), List('i', 'j', 'k', 'l'), List('m', 'n'), List('o')))
    assert(List(List('i', 'j', 'k', 'l'), List('o'), List('a', 'b', 'c'),  List('f', 'g', 'h'), List('d', 'e'), List('d', 'e'), List('m', 'n') )
      === result)
  }
  